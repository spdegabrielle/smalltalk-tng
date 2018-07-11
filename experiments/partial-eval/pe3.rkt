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

;; PE translates an input AST plus an Environment into an AbsVal,
;; while recording a sequential (!) *history* of named computations.

;; AST
(record Lit (value)) ;; (Lit RacketAtom)
(record Prim (name handler)) ;; (Prim Symbol HandlerFun)
(record Ref (id)) ;; (Ref Symbol)
(record If (test true false)) ;; (If AST AST AST)
(record Lambda (formals body)) ;; (Lambda (Listof Symbol) AST)
(record Apply (rator rands)) ;; (Apply AST (Listof AST))
(record Bind (formal init body)) ;; (Bind Symbol AST AST)
(record Letrec (formals inits body)) ;; (Letrec (Listof Symbol) (Listof AST) AST)

;; An AbsVal is an *abstract value*, one of
;;  - (Unknown Symbol), for a completely unknown value whose
;;    computation is named by Symbol in the history;
;;  - (Runtime Symbol Description), for a partially- or
;;    completely-known value whose computation is named by Symbol in the
;;    history; or
;;  - (Compiletime Description), for a completely-known value whose
;;    computation does not and need not appear in the history.
;;
(record Unknown (reference))
(record Runtime (reference description))
(record Compiletime (description))

;; An Environment maps user-level variable names to abstract-values.
;; Environment = (Listof (Cons Symbol AbsVal))

;; Description
(record Atom (value)) ;; (Atom RacketAtom)
;; ... or Prim; or ...
(record Pair (car cdr)) ;; (Pair AbsVal AbsVal)
(record Closure (formals body env)) ;; (Closure (Listof Symbol) AST Environment)

;;---------------------------------------------------------------------------

(define-match-expander Known
  (syntax-rules ()
    [(_ desc)
     (or (Runtime _ desc)
         (Compiletime desc))]))

(define (known? v)
  (or (Compiletime? v)
      (Runtime? v)))

(define (known-value v)
  (match v [(Known d) d]))

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

(define (extend-env env names inits)
  (append (for/list [(name names) (init inits)] (cons name (box init))) env))

(define (lookup-env env name)
  (match (assq name env)
    [(cons _name (box absval)) absval]
    [#f (let loop ((hs (pe-history)))
          (match hs
            ['()
             (unbox (hash-ref *globals*
                              name
                              (lambda () (error 'lookup-env "Unbound variable: ~v" name))))]
            [(cons h hs)
             (match (assq name h)
               [(list _name _ast absval) absval]
               [#f (loop hs)])]))]))

;;---------------------------------------------------------------------------

;; pe-history : (Parameterof History)
;; History = (Listof Era)
;; Era = (Listof (List Symbol AST AbsVal))
(define pe-history (make-parameter '()))

(define (next-id base) (gensym base))

(define-syntax-rule (residualize expr)
  (parameterize ((pe-history (cons '() (pe-history))))
    (define av expr)
    (wrap-era (car (pe-history)) (codegen-absval av))))

(define (wrap-era h body)
  (match h
    ['() body]
    [(cons (list id ast av) h)
     (if (equal? body (Ref id))
         (wrap-era h ast)
         (wrap-era h (Bind id ast body)))]))

(define-syntax-rule (emit [id ast-expr] av-expr)
  (let* ((id (next-id 'id))
         (ast ast-expr)
         (av av-expr))
    (match-define (cons h hs) (pe-history))
    (pe-history (cons (cons (list id ast av) h) hs))
    av))

(define (codegen-absval absval)
  (match absval
    [(Unknown id) (Ref id)]
    [(Runtime id _) (Ref id)]
    [(Compiletime d) (codegen-desc d)]))

(define (codegen-desc d)
  (match d
    [(Atom v) (Lit v)]
    [(? Prim? p) p]
    [(Pair a d) (Apply CONS-prim (map codegen-absval (list a d)))]
    [(Closure formals body env) (Lambda formals body)]))

(define INDENT (make-parameter 0))
(define (D x)
  (when #f ;; #t
    (display (make-string (INDENT) #\space))
    (display x)
    (newline)))

;; AST Environment -> AbsVal
(define (pe pexp env)
  (D `((pexp ,pexp) (env ,env)))
  (parameterize ((INDENT (+ (INDENT) 2)))
  (match pexp
    [(Lit v) (Compiletime (Atom v))]
    [(Prim _ _) (Compiletime pexp)]
    [(Ref id) (lookup-env env id)]

    [(If test true false)
     (match (pe test env)
       [(Known (Atom #f)) (pe false env)]
       [(Known _) (pe true env)]
       [(Unknown test-id)
        (emit [if-id (If (Ref test-id)
                         (residualize (pe true env))
                         (residualize (pe false env)))]
              (Unknown if-id))])]

    [(Lambda formals body)
     (define (non-global? id) (assq id env))
     (define captured (filter non-global? (set->list (free-names pexp))))
     (define cloenv (extend-env '() captured (for/list [(c captured)] (lookup-env env c))))
     (define new-body (residualize (pe body (extend-env cloenv formals (map Unknown formals)))))
     (define clo (Closure formals new-body cloenv))
     (emit [lam-id (codegen-desc clo)]
           (Runtime lam-id clo))]

    [(Apply rator rands)
     (define rator-v (pe rator env))
     (define rands-vs (for/list [(rand rands)] (pe rand env)))
     (match rator-v
       [(Known (Closure formals body cloenv))
        (D `(--> closure ,body))
        (pe body (extend-env cloenv formals rands-vs))]
       [(Known (and prim (Prim _name handler)))
        (D `(--> prim ,_name ,@rands-vs))
        (apply handler prim rands-vs)]
       [(Unknown rator-id)
        (emit [app-id (Apply (Ref rator-id) (map codegen-absval rands-vs))]
              (Unknown app-id))])]

    [(Bind formal init body)
     (define init-v (pe init env))
     (pe body (extend-env env (list formal) (list init-v)))]

    [(Letrec formals inits body)
     (error 'pe "Unimplemented: Letrec")])
  ))

;;---------------------------------------------------------------------------

(define (prim-app prim . args)
  (emit [app-id (Apply prim (map codegen-absval args))]
        (Unknown app-id)))

(define (lift-residualize f)
  (lambda (self . args)
    (if (andmap known? args)
        (Compiletime (Atom (apply f (map unatom args))))
        (apply prim-app self args))))

(define (lift-residualize* f)
  (lambda (self . args)
    (if (andmap known? args)
        (Compiletime (Atom (apply f args)))
        (apply prim-app self args))))

(define (lift-commutative-associative-binop f identity)
  (lambda (self . vals)
    (define-values (known unknown) (partition known? vals))
    (define part-val (apply f (map unatom known)))
    (define part (Compiletime (Atom part-val)))
    (cond [(null? unknown) part]
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

           (Prim 'null? (lift-residualize* (match-lambda [(Known (Atom '())) #t] [_ #f])))
           (Prim 'pair? (lift-residualize* (match-lambda [(Known (Pair _ _)) #t] [_ #f])))

           (Prim 'error prim-app) ;; strictly residualized

           ;; (Prim 'number? (lift-residualize* 'number? (lambda (x) (and (Lit? x) (number? (Lit-value x))))))
           ;; (Prim 'zero? (lift-residualize 'zero? zero?))
           ;; (Prim 'eq? (lambda (x y)
           ;;              (if (and (Lit? x)
           ;;                       (Lit? y))
           ;;                  (Lit (eq? (Lit-value x) (Lit-value y)))
           ;;                  (prim-app 'eq? x y))))

           (Prim 'PRIMcar (lambda (self x)
                            (match x
                              [(Known (Pair a _)) a]
                              [_ (prim-app self x)])))
           (Prim 'PRIMcdr (lambda (self x)
                            (match x
                              [(Known (Pair _ d)) d]
                              [_ (prim-app self x)])))
           ))

(define (extend-globals! entry)
  (match-define (list global-name global-source) entry)
  (parameterize ((pe-history (cons '() (pe-history))))
    (match (pe (parse global-source) '())
      [(Unknown _)
       (error 'extend-globals! "Global ~v produced unknown result" global-name)]
      [(Known (? Closure? c))
       (when (not (set-empty? (free-names (codegen-desc c))))
         (error 'extend-globals! "Global ~v produced non-empty closure: ~a"
                global-name
                (free-names (codegen-desc c))))
       (hash-set! *globals* (car entry) (box (Runtime global-name c)))]
      [(Known other)
       (error 'extend-globals! "Global ~v produced non-closure: ~v" global-name other)])))

(for-each extend-globals!
          (list
           (list 'car '(lambda (x)
                         (if (pair? x)
                             (PRIMcar x)
                             (error "Not a pair in car" x))))
           (list 'cdr '(lambda (x)
                         (if (pair? x)
                             (PRIMcdr x)
                             (error "Not a pair in cdr" x))))

           ;; (list 'reverse '(lambda (x)
           ;;                   #:filter 'unfold
           ;;                   (let loop ((x x) (acc '()))
           ;;                     (if (null? x)
           ;;                         acc
           ;;                         (loop (cdr x) (cons (car x) acc))))))
           ;; (list 'fold '(lambda (f acc x)
           ;;                #:filter (if (sval-known? f) 'unfold '(#f #f #f))
           ;;                (let loop ((x x) (acc acc))
           ;;                  (if (null? x)
           ;;                      acc
           ;;                      (loop (cdr x) (f (car x) acc))))))
           ;; (list 'fold-right '(lambda (f acc x)
           ;;                      #:filter (if (sval-known? f) 'unfold '(#f #f #f))
           ;;                      (let loop ((x x))
           ;;                        (if (null? x)
           ;;                            acc
           ;;                            (let ((head (car x)))
           ;;                              (f head (loop (cdr x))))))))
           ;; (list 'list? '(lambda (xs)
           ;;                 (let loop ((xs xs))
           ;;                   (if (null? xs)
           ;;                       #t
           ;;                       (if (pair? xs)
           ;;                           (loop (cdr xs))
           ;;                           #f)))))
           ;; (list 'map '(lambda (f x)
           ;;               #:filter 'unfold
           ;;               (fold-right (lambda (v c)
           ;;                             #:filter 'unfold
           ;;                             (cons (f v) c)) '() x)))
           ;; (list 'append '(lambda (a b)
           ;;                  #:filter 'unfold
           ;;                  (fold-right cons b a)))
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

  (define (reconstruct v)
    (match v
      [(Lit v)
       `(quote ,v)]
      [(Prim name _)
       (string->symbol (string-append "#%" (symbol->string name)))]
      [(Ref id)
       id]
      [(If test true false)
       `(if ,(reconstruct test) ,(reconstruct true) ,(reconstruct false))]
      [(Lambda formals body)
       `(lambda ,formals ,(reconstruct body))]
      [(Apply rator rands)
       `(,(reconstruct rator) ,@(map reconstruct rands))]
      [(Bind formal init body)
       (reconstruct-binds (list (list formal (reconstruct init))) body)]
      ))

  (define (reconstruct-binds bs body)
    (match body
      [(Bind formal init body) (reconstruct-binds (cons (list formal (reconstruct init)) bs) body)]
      [_ `(let* ,(reverse bs) ,(reconstruct body))]))

  (define (T e)
    (define ast (residualize (time (pe (parse e) '()))))
    ;; (pretty-display ast)
    (pretty-display (reconstruct ast)))

  (define add1-to-123-exp
    '((lambda (x) (+ x 1)) 123))

  (define add1-to-123-exp1
    `(let ((c (lambda (f) (f 123))))
       (c (lambda (x) (+ x 1)))))

  (define cdar-exp `(let ((p (cons 1 2))) (PRIMcdr (PRIMcar (cons p p)))))
  (define cdar-exp1 `(let ((p (cons 1 2))) (cdr (car (cons p p)))))
  (define cdar-exp2 `(lambda (p) (cdr (car (cons p p)))))

  (define curried-exp
    '(lambda (do-something-with bb)
       ((((lambda (a)
            (lambda (b)
              (lambda (c)
                (do-something-with a b c))))
          'aa)
         (bb))
        'cc)))

  (define curried-cps-exp
    '(lambda (do-something-with bb)
       ((lambda (k a) (k (lambda (k b) (k (lambda (k c) (do-something-with k a b c))))))
        (lambda (bf) (bf (lambda (cf) (cf (lambda (x) (begin x))
                                          'cc))
                         (bb)))
        'aa)))

  (define shrunk-curried-exp
    '(lambda (bb)
       ((lambda (k b) (k (lambda () b)))
        (lambda (f) (f))
        (bb))))

  (define code-duplication-exp
    '(lambda (f g h a b c)
       (let ((x (f a b c)))
         (let ((y (g x x)))
           (h y y)))))

  (define unknown-if-with-computation
    '(lambda (p f g x)
       (if (p x)
           (f (g x))
           (g (f x)))))
  )
