#lang racket/base
;; Another restart. Started 2018-07-08.

(require racket/set)
(require racket/match)
(require racket/pretty)
(require (only-in racket/list drop-right last partition append-map))
(require (only-in racket/struct make-constructor-style-printer))

(module+ test (require rackunit))

(define-syntax-rule (record N (F ...) extra ...)
  (struct N (F ...) #:transparent extra ...))

;; Input AST
(record Lit (value)) ;; (Lit RacketAtom)
(record Prim (name handler)) ;; (Prim Symbol HandlerFun)
(record Ref (id)) ;; (Ref Symbol)
(record If (test true false)) ;; (If AST AST AST)
(record Lambda (formals body)) ;; (Lambda (Listof Symbol) AST)
(record Apply (rator rands)) ;; (Apply AST (Listof AST))
(record Bind (formal init body)) ;; (Bind Symbol AST AST)
(record Letrec (formals inits body)) ;; (Letrec (Listof Symbol) (Listof AST) AST)

;; ;; Names
;; (record Impure (id))
;; (record Pure (op args))
;; (define *impure-counter* 0)
;; (define (impure) (Impure (begin0 *impure-counter* (set! *impure-counter* (+ *impure-counter* 1)))))
;; (define (pure op . args) (Pure op args))
;; ;; Abstract values
;; (record Value (name residual details))

;; PE translates an input AST plus an Environment into a sequence (!)
;; of named computations, plus a final abstract-value.
;;
;; An abstract-value is a pair of
;;  - a residual, which is either:
;;      - a variable reference, if some run-time computation/allocation is involved, or
;;      - #f, if the abstract-value is entirely compile-time;
;;    and
;;  - a description of everything known about the value produced by
;;    the value the residual computation would yield.
;;
;; An Environment maps user-level variable names to abstract-values.
;;
;; Environment = (Listof (Cons Symbol AbsVal))

(record Answer (computations value)) ;; (Answer (Listof (List Symbol AST AbsVal)) AbsVal)

;; AbsVal
(record Runtime (reference description)) ;; (Runtime Symbol (U Description Unknown))
(record Compiletime (description)) ;; (Compiletime Description)

;; Unknown
(record Unknown ()) ;; (Unknown)

;; Descriptions
(record Atom (value)) ;; (Atom RacketAtom)
;; Prim
(record Pair (car cdr)) ;; (Pair AbsVal AbsVal)
(record Closure (formals body env)) ;; (Closure (Listof Symbol) AST Environment)

(define (next-id base) (gensym base))

(define (known-case v k-known k-unknown)
  (match v
    [(Runtime r (Unknown)) (k-unknown r)]
    [(Runtime r d) (k-known r d)]
    [(Compiletime d) (k-known #f d)]))

(define (known? v)
  (or (Compiletime? v)
      (and (Runtime? v)
           (not (Unknown? (Runtime-description v))))))

(define (known-value v)
  (match v
    [(Compiletime d) d]
    [(Runtime _ d) d]))

(define (unatom v)
  (Atom-value (known-value v)))

;;---------------------------------------------------------------------------

(define (parse exp)
  (let walk ((exp exp))
    (match exp
      [(? symbol?) (Ref exp)]

      [`(quote ,e) (Lit e)]

      [`(when ,test ,es ...) (If (walk test) (walk `(begin ,@es)) (Lit (void)))]
      [`(if ,test ,true ,false) (If (walk test) (walk true) (walk false))]

      [`(cond (else ,es ...)) (walk `(begin ,@es))]
      [`(cond (,test ,es ...) ,clauses ...) (walk `(if ,test (begin ,@es) (cond ,@clauses)))]

      [`(lambda (,formals ...) ,body-exps ...) (Lambda formals (walk `(begin ,@body-exps)))]

      [`(begin) (Lit (void))]
      [`(begin ,e) (walk e)]
      [`(begin ,e ,es ...) (walk `(let ((,(gensym 'ignored) ,e)) ,@es))]

      [`(let ,(? symbol? name) ((,names ,inits) ...) ,es ...)
       (walk `(letrec ((,name (lambda ,names ,@es))) (,name ,@inits)))]
      [`(let ((,names ,inits) ...) ,es ...)
       (walk `((lambda ,names ,@es) ,@inits))]

      [`(let* () ,es ...)
       (walk `(begin ,@es))]
      [`(let* ((,name ,init) ,more ...) ,es ...)
       (walk `(let ((,name ,init)) (let* ,more ,@es)))]

      [`(,rator ,rands ...)
       (Apply (walk rator) (map walk rands))]

      [`(letrec ((,names ,inits) ...) ,es ...)
       (if (null? names)
           (walk `(begin ,@es))
           (Letrec names (map walk inits) (walk `(begin ,@es))))]

      [_ (Lit exp)])))

;;---------------------------------------------------------------------------

(define (free-names pexp)
  (match pexp
    [(Lit _) (seteq)]
    [(Prim _ _) (seteq)]
    [(Ref id) (seteq id)]
    [(If test true false) (set-union (free-names test) (free-names true) (free-names false))]
    [(Lambda formals body)
     (set-subtract (free-names body)
                   (list->seteq formals))]
    [(Apply rator rands) (apply set-union (free-names rator) (map free-names rands))]
    [(Bind formal init body) (set-union (set-remove (free-names body) formal) (free-names init))]
    [(Letrec formals inits body)
     (set-subtract (apply set-union (free-names body) (map free-names inits))
                   (list->seteq formals))]))

;;---------------------------------------------------------------------------

(define *globals* (make-hash))

;; (define (extend-env/undefined env names)
;;   (append (for/list [(name names)] (cons name (box #f))) env))

(define (extend-env env names inits)
  (append (for/list [(name names) (init inits)] (cons name (box init))) env))

(define (extend-env/computations env cs)
  (append (for/list [(c cs)]
            (match-define (list id ast av) c)
            (cons id (box av)))
          env))

(define (lookup-env env name)
  (match (assq name env)
    [#f (unbox (hash-ref *globals*
                         name
                         (lambda () (error 'lookup-env "Unbound variable: ~v" name))))]
    [(cons _name (box absval)) absval]))

;;---------------------------------------------------------------------------

(define (pe-step pexp env k)
  (match (pe pexp env)
    [(Answer cs1 absval)
     (match (k absval (extend-env/computations env cs1))
       [(Answer cs2 final-absval)
        (D `(gluing ,cs1 ,cs2))
        (Answer (append cs1 cs2) final-absval)])]))

(define (return absval)
  (Answer '() absval))

(define-syntax-rule (emit [id ast-expr] av-expr)
  (let* ((ast ast-expr)
         (id (next-id 'id))
         (av av-expr))
    (D `(emitting ,id ,ast ,av))
    (Answer (list (list id ast av)) av)))

(define (codegen ans)
  (match-define (Answer defs absval) ans)
  (let loop ((defs defs))
    (match defs
      ['() (codegen-absval absval)]
      [(cons (list id ast av) defs)
       (Bind id ast (loop defs))])))

(define (codegen-absval absval)
  (known-case absval
              (lambda (id d)
                (if id
                    (Ref id)
                    (codegen-desc d)))
              (lambda (id)
                (Ref id))))

(define (codegen-desc d)
  (match d
    [(Atom v) (Lit v)]
    [(? Prim? p) p]
    [(Pair a d) (Apply CONS-prim (map codegen-desc (list a d)))]
    [(Closure formals body env) (Lambda formals body)]))

(define INDENT (make-parameter 0))
(define (D x)
  (when #t
    (display (make-string (INDENT) #\space))
    (display x)
    (newline)))

;; AST Environment -> Answer
(define (pe pexp env)
  (D `((pexp ,pexp) (env ,env)))
  (parameterize ((INDENT (+ (INDENT) 2)))
  (match pexp
    [(Lit v) (return (Compiletime (Atom v)))]

    [(Prim _ _) (return (Compiletime pexp))]

    [(Ref id) (return (lookup-env env id))]

    [(If test true false)
     (pe-step test env
              (lambda (test-v env)
                (known-case test-v
                            (lambda (test-id test-d)
                              (if (equal? test-d (Atom #f))
                                  (pe false env)
                                  (pe true env)))
                            (lambda (test-id)
                              (emit [if-id (If (Ref test-id)
                                               (codegen (pe true env))
                                               (codegen (pe false env)))]
                                    (Runtime if-id (Unknown)))))))]

    [(Lambda formals body)
     (define cloenv
       (let ((free (filter (lambda (i) (assq i env)) ;; only the non-global names
                           (set->list (free-names pexp)))))
         (extend-env '()
                     free
                     (for/list [(f free)] (lookup-env env f)))))
     (define clo (Closure formals
                          (codegen (pe body
                                       (extend-env cloenv
                                                   formals
                                                   (for/list [(formal formals)]
                                                     (Runtime formal (Unknown))))))
                          cloenv))
     (emit [lam-id (codegen-desc clo)]
           (Runtime lam-id clo))]

    [(Apply rator rands)
     (pe-step rator env
              (lambda (rator-v env)
                (let loop ((env env) (rands rands) (rands-vs-rev '()))
                  (match rands
                    ['()
                     (define rands-vs (reverse rands-vs-rev))
                     (known-case rator-v
                                 (lambda (rator-id d)
                                   (match d
                                     [(Closure formals body cloenv)
                                      (D `(--> closure ,body))
                                      (pe body (extend-env cloenv formals rands-vs))]
                                     [(Prim _name handler)
                                      (D `(--> prim ,_name ,@rands-vs))
                                      (apply handler d rands-vs)]))
                                 (lambda (rator-id)
                                   (emit [app-id (Apply (Ref rator-id)
                                                        (map codegen-absval rands-vs))]
                                         (Runtime app-id (Unknown)))))]
                    [(cons rand rands)
                     (pe-step rand env
                              (lambda (rand-v env)
                                (loop env rands (cons rand-v rands-vs-rev))))]))))]

    [(Bind formal init body)
     (pe-step init env
              (lambda (init-v env)
                (pe body (extend-env env (list formal) (list init-v)))))]

    [(Letrec formals inits body)
     (error 'pe "Unimplemented: Letrec")])
  ))

;;---------------------------------------------------------------------------

(define (prim-app prim . args)
  (emit [app-id (Apply prim (map codegen-absval args))]
        (Runtime app-id (Unknown))))

(define (lift-residualize f)
  (lambda (self . args)
    (if (andmap known? args)
        (return (Compiletime (Atom (apply f (map unatom args)))))
        (apply prim-app self args))))

(define (lift-residualize* f)
  (lambda (self . args)
    (if (andmap known? args)
        (return (Compiletime (Atom (apply f args))))
        (apply prim-app self args))))

(define (lift-commutative-associative-binop f identity)
  (lambda (self . vals)
    (define-values (known unknown) (partition known? vals))
    (define part-val (apply f (map unatom known)))
    (define part (Compiletime (Atom part-val)))
    (cond [(null? unknown) (return part)]
          [(= identity part-val) (apply prim-app self unknown)]
          [else (apply prim-app self part unknown)])))

(define CONS-prim (Prim 'cons (lambda (self a d)
                                (emit [pair-id (Apply CONS-prim (map codegen-absval (list a d)))]
                                      (Runtime pair-id (Pair a d))))))

(for-each (lambda (p) (hash-set! *globals* (Prim-name p) (box (Compiletime p))))
          (list
           (Prim '+ (lift-commutative-associative-binop + 0))
           (Prim '* (lift-commutative-associative-binop * 1))
           (Prim '- (lift-residualize -))
           (Prim '< (lift-residualize <))

           CONS-prim

           ;; (Prim 'null? (lift-residualize* 'null? (lambda (x) (and (Lit? x) (null? (Lit-value x))))))
           ;; (Prim 'pair? (lift-residualize* 'pair? (lambda (x)
           ;;                                          (or (Cons? x)
           ;;                                              (and (Lit? x) (pair? (Lit-value x)))))))
           ;; (Prim 'number? (lift-residualize* 'number? (lambda (x) (and (Lit? x) (number? (Lit-value x))))))
           ;; (Prim 'zero? (lift-residualize 'zero? zero?))
           ;; (Prim 'eq? (lambda (x y)
           ;;              (if (and (Lit? x)
           ;;                       (Lit? y))
           ;;                  (Lit (eq? (Lit-value x) (Lit-value y)))
           ;;                  (prim-app 'eq? x y))))

           (Prim 'PRIMcar (lambda (self x)
                            (if (known? x)
                                (return (Pair-car (known-value x)))
                                (prim-app self x))))
           (Prim 'PRIMcdr (lambda (self x)
                            (if (known? x)
                                (return (Pair-cdr (known-value x)))
                                (prim-app self x))))
           ))

;;---------------------------------------------------------------------------

;; (require racket/trace) (trace pe)

(module+ test
  (define compose-exp
    '(let ((compose (lambda (f g)
                      (lambda (x)
                        (f (g x))))))
       (compose (lambda (a) (* a 2))
                (lambda (b) (+ b 1)))))

  (define (reconstruct-ast v)
    (match v
      ;; [(Closure formals body '())
      ;;  `(lambda ,formals ,(reconstruct-ast body))]
      ;; [(Closure formals body env)
      ;;  `(let (,@(map (lambda (entry)
      ;;                  (match-define (cons name id) entry)
      ;;                  (list name id))
      ;;                env))
      ;;     (lambda ,formals ,(reconstruct-ast body)))]
      [(Lit v)
       `(quote ,v)]
      [(Prim name _)
       `(PRIM ,name)]
      [(Ref id)
       id]
      [(If test true false)
       `(if ,(reconstruct-ast test) ,(reconstruct-ast true) ,(reconstruct-ast false))]
      [(Lambda formals body)
       `(lambda ,formals ,(reconstruct-ast body))]
      [(Apply rator rands)
       `(,(reconstruct-ast rator) ,@(map reconstruct-ast rands))]
      [(Bind formal init body)
       `(let ((,formal ,(reconstruct-ast init))) ,(reconstruct-ast body))]
      ))

  (define (reconstruct ans)
    (match-define (Answer history absval) ans)
    `(let* (,@(map (lambda (entry)
                     (match-define (list id ast av) entry)
                     (list id (reconstruct-ast ast)))
                   history))
       ,(reconstruct-ast (codegen-absval absval))))

  (define (T e)
    (define ans (pe (parse e) '()))
    ;; (pretty-display ans)
    (pretty-display (reconstruct ans)))

  ;; (T '((lambda (x) (+ x 1)) 123))
  ;; (T `(let ((c (lambda (f) (f 123))))
  ;;       (c (lambda (x) (+ x 1)))))

  ;; (T `(let ((p (cons 1 2))) (PRIMcdr (PRIMcar (cons p p)))))

  ;; (T compose-exp)

  ;; (T
  ;;  '(lambda (do-something-with bb)
  ;;     ((lambda (k a) (k (lambda (k b) (k (lambda (k c) (do-something-with k a b c))))))
  ;;      (lambda (bf) (bf (lambda (cf) (cf (lambda (x) (begin x))
  ;;                                        'cc))
  ;;                       (bb)))
  ;;      'aa)))

  (T
   '(lambda (bb)
      ((lambda (k b) (k (lambda () b)))
       (lambda (f) (f))
       (bb))))
  )
