#lang racket/base
;; Similar to the partial-evaluator in "Partial Evaluator as a
;; Compiler for Reflective Languages", Asai, Masuhara, Matsuoka and
;; Yonezawa (1995), but without preactions (yet).
;;
;; Copyright (C) 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;;
;; Updated 2018 from plain Scheme to Racket (v6.90). Major change is
;; using Racket `struct`s and pattern-matching instead of ad-hoc
;; "node" structures.

(require racket/set)
(require racket/match)
(require racket/pretty)
(require (only-in racket/list drop-right last partition append-map))

(define-syntax-rule (define-node-struct N (F ...))
  (struct N (F ...) #:prefab))

(define-node-struct Lit (value))
(define-node-struct Letrec (names inits body))
(define-node-struct Ref (name))
(define-node-struct Lambda (formals filter body))
(define-node-struct Begin (exprs))
(define-node-struct If (test true false))
(define-node-struct Apply (rator rands))
(define-node-struct Closure (formals filter body env))
(define-node-struct ApplyCached (rator rands))
(define-node-struct Prim (name handler))
(define-node-struct Cons (a d))

(define (parse exp env)
  (let walk ((exp exp))
    (match exp
      [(? symbol?) (Ref exp)]

      [`(quote ,e) (Lit e)]

      [`(lambda (,formals ...) #:filter ,filter ,body-exps ...)
       (define newenv (append formals env))
       (Lambda formals
               (parse filter newenv)
               (parse `(begin ,@body-exps) newenv))]
      [`(lambda (,formals ...) ,body-exps ...)
       (Lambda formals
               #f
               (parse `(begin ,@body-exps) (append formals env)))]
      [`(lambda* (,formals ...) ,body-exps ...)
       (Lambda formals
               (Lit 'unfold)
               (parse `(begin ,@body-exps) (append formals env)))]

      [`(begin) (Lit (void))]
      [`(begin ,e) (walk e)]
      [`(begin ,es ...) (Begin (map walk es))]

      [`(if ,test ,true) (If (walk test) (walk true) (Lit (void)))]
      [`(if ,test ,true ,false) (If (walk test) (walk true) (walk false))]

      [`(cond) (Lit (void))]
      [`(cond (else ,es ...)) (walk `(begin ,@es))]
      [`(cond (,test ,es ...) ,clauses ...) (walk `(if ,test (begin ,@es) (cond ,@clauses)))]

      [`(let ,(? symbol? name) ((,names ,inits) ...) ,es ...)
       (walk `(letrec ((,name (lambda ,names ,@es))) (,name ,@inits)))]
      [`(let ((,names ,inits) ...) ,es ...)
       (walk `((lambda* ,names ,@es) ,@inits))]

      [`(letrec ((,names ,inits) ...) ,es ...)
       (if (null? names)
           (walk `(begin ,@es))
           (let* ((newenv (append names env))
                  (p (lambda (x) (parse x newenv))))
             (Letrec names (map p inits) (p `(begin ,@es)))))]

      [`(let* () ,es ...)
       (walk `(begin ,@es))]
      [`(let* ((,name ,init) ,more ...) ,es ...)
       (walk `(let ((,name ,init)) (let* ,more ,@es)))]

      [`(,rator ,rands ...)
       (Apply (walk rator) (map walk rands))]

      [_ (Lit exp)])))

(define (extend-env/undefined env names)
  (append (for/list [(name names)] (cons name (box #f))) env))

(define (extend-env env names inits)
  (append (for/list [(name names) (init inits)] (cons name (box (list init)))) env))

(define (sval-known? node)
  (or (Lit? node)
      (Cons? node)
      (Prim? node)
      (Closure? node)))

(define (pe pexp env cache)
  ;;(pretty-print `(pe (pexp ,pexp) ));; (env ,env) (cache ,cache)))
  (match pexp
    [(Lit _) pexp]

    [(Letrec names inits body)
     (define newenv (extend-env/undefined env names))
     (for [(name names) (init inits)]
       (define binding-box (cdr (assq name newenv)))
       (set-box! binding-box (list (pe init newenv cache))))
     ;; BUG: need to collect free variables of the pe'd body, and if
     ;; there are references to any of this pexp's names, we need to
     ;; wrap the pe'd body in a letrec of the relevant names.
     ;; (Actually, iterate until we've pulled in everything we need.)
     (pe body newenv cache)]

    [(Ref name)
     (match (assq name env)
       [#f pexp] ;; TODO: unbound variable error instead?
       [(cons _ (box #f)) pexp] ;; TODO: reference to not-yet-initialized-variable error instead?
       [(cons _ (box (list v))) v])]

    [(Lambda formals filter body) (Closure formals filter body env)]

    [(Begin exprs)
     (for [(exp (drop-right exprs 1))]
       (log-warning ";; Ignoring ~v" exp))
     (pe (last exprs) env cache)]

    [(If test true false)
     (define val (pe test env cache))
     (if (sval-known? val)
         (if (and (Lit? val) (Lit-value val))
             (pe true env cache)
             (pe false env cache))
         (If val
             (pe true env cache)
             (pe false env cache)))]

    [(Apply rator0 rands0)
     (define rator (pe rator0 env cache))
     (define rands (for/list [(r rands0)] (pe r env cache)))
     (if (sval-known? rator)
         (match rator
           [(Closure formals filter body rator-env)
            (define newenv (extend-env rator-env formals rands))
            (match (cond [filter (Lit-value (pe filter newenv cache))]
                         [(andmap sval-known? rands) 'unfold]
                         [else (for/list [(f formals)] #f)])
              ['unfold (pe body newenv cache)]
              [(list (? boolean? props) ...)
               (define cache-rands (for/list [(prop props) (formal formals) (rand rands)]
                                     (if prop rand (Ref formal))))
               (define cache-key (cons rator cache-rands))
               (match (assoc cache-key cache)
                 [(list _ cached-rator) (ApplyCached cached-rator rands)]
                 [#f
                  (define tmp-sym (gensym 'rator))
                  (Letrec (list tmp-sym)
                          (list (Closure formals
                                         filter
                                         (pe body
                                             (extend-env rator-env formals cache-rands)
                                             (cons (list cache-key (Ref tmp-sym)) cache))
                                         rator-env))
                          (Apply (Ref tmp-sym) rands))])])]
           [(Prim name handler)
            ;;(pretty-print `(prim ,name ));; ,@rands))
            (apply handler rands)])
         (Apply rator rands))]))

(define (codegen0 pexp)
  (match pexp
    [(Lit v) `(quote ,v)]

    [(Cons a (Lit '())) `(LIST ,(codegen a))]
    [(Cons a d)
     (match (codegen d)
       [(cons 'LIST r) `(LIST ,(codegen a) ,@r)]
       [dc `(cons ,(codegen a) ,dc)])]

    [(Prim name _) name]

    [(Closure formals #f body _env)
     `(lambda ,formals ,(codegen body))]
    [(Closure formals filter body _env)
     `(lambda ,formals #:filter ,(codegen filter) ,(codegen body))]

    [(Letrec names inits body)
     `(letrec ,(for/list [(name names) (init inits)] (list name (codegen init))) ,(codegen body))]

    [(Ref name) name]

    [(Lambda formals #f body) `(lambda ,formals ,(codegen body))]
    [(Lambda formals filter body) `(lambda ,formals #:filter ,(codegen filter) ,(codegen body))]

    [(Begin exprs) `(begin ,@(map codegen exprs))]

    [(If test true false) `(if ,(codegen test) ,(codegen true) ,(codegen false))]

    [(Apply rator rands) `(,(codegen rator) ,@(map codegen rands))]
    [(ApplyCached rator rands) `(GOTO ,(codegen rator) ,@(map codegen rands))]))

(define (free-names pexp)
  (match pexp
    [(Lit _) (seteq)]
    [(Cons a d) (set-union (free-names a) (free-names d))]
    [(Prim _ _) (seteq)]
    [(or (Closure formals _ body _)
         (Lambda formals _ body))
     (set-subtract (free-names body) (list->seteq formals))]
    [(Letrec names inits body)
     (set-subtract (apply set-union (free-names body) (map free-names inits)) (list->seteq names))]
    [(Ref name) (seteq name)]
    [(Begin exprs) (apply set-union (seteq) (map free-names exprs))]
    [(If test true false) (set-union (free-names test) (free-names true) (free-names false))]
    [(or (Apply rator rands)
         (ApplyCached rator rands))
     (apply set-union (free-names rator) (map free-names rands))]))

(struct cached-code ([use-count #:mutable] temp-var-sym [reduced-expr #:mutable]) #:transparent)

(define (codegen pexp)
  (define cache (make-hash)) ;; (HashTable pexp cached-code)

  (define (walk pexp)
    (match (hash-ref cache pexp #f)
      [#f
       (define c (cached-code 1 (gensym 't) #f))
       (hash-set! cache pexp c)
       (set-cached-code-reduced-expr! c (walk1 pexp))
       (cached-code-temp-var-sym c)]
      [c
       (set-cached-code-use-count! c (+ (cached-code-use-count c) 1))
       (cached-code-temp-var-sym c)]))

  (define (env-entry-walker names)
    (lambda (entry)
      (match-define (cons name (box (list v))) entry)
      (if (set-member? names name)
          (list (list name (walk v)))
          '())))

  (define (walk1 pexp)
    (match pexp
      [(Lit v) `(quote ,v)]

      [(Cons a (Lit '())) `(LIST ,(walk a))]
      [(Cons a d)
       (match (walk d)
         [(cons 'LIST r) `(LIST ,(walk a) ,@r)]
         [dc `(cons ,(walk a) ,dc)])]

      [(Prim name _) name]

      [(Closure formals filter body env)
       `(let (,@(append-map (env-entry-walker (free-names pexp)) env))
          (lambda ,formals
            ,@(if filter
                  `(#:filter ,(walk filter))
                  `())
            ,(walk body)))]

      [(Letrec names inits body)
       `(letrec ,(for/list [(name names) (init inits)] (list name (walk init))) ,(walk body))]

      [(Ref name) name]

      [(Lambda formals #f body) `(lambda ,formals ,(walk body))]
      [(Lambda formals filter body) `(lambda ,formals #:filter ,(walk filter) ,(walk body))]

      [(Begin exprs) `(begin ,@(map walk exprs))]

      [(If test true false) `(if ,(walk test) ,(walk true) ,(walk false))]

      [(Apply rator rands) `(,(walk rator) ,@(map walk rands))]
      [(ApplyCached rator rands) `(GOTO ,(walk rator) ,@(map walk rands))]))

  (let* ((exp (walk pexp))
         (remapped-cache (for/hash [(c (in-hash-values cache))]
                           (values (cached-code-temp-var-sym c) c))))

    (define (inlinable? c)
      (match-define (cached-code use-count temp-var-sym reduced-expr) c)
      (or (= use-count 1)
          #t ;; TODO: this is here for temporary experimentation
          #;(match reduced-expr
            [(? symbol?) #t]
            [`(quote ,(? symbol?)) #f]
            [`(quote ,v) #t]
            [_ #f])
          ))

    (define (inline exp)
      (cond
        [(pair? exp) (cons (inline (car exp)) (inline (cdr exp)))]
        [(hash-ref remapped-cache exp #f) =>
         (lambda (c)
           (if (inlinable? c)
               (inline (cached-code-reduced-expr c))
               exp))]
        [else exp]))

    `(codegen-result ,(inline exp)
                     ;; ,remapped-cache
                     ,(for/list [(c (in-hash-values remapped-cache)) #:when (not (inlinable? c))] c)
                     )))

(define (residualize-apply name . args) (Apply (Ref name) args))

(define (lift-residualize f-name f)
  (lambda args
    (if (andmap sval-known? args)
        (Lit (apply f (map Lit-value args)))
        (apply residualize-apply f-name args))))

(define (lift-residualize* f-name f)
  (lambda args
    (if (andmap sval-known? args)
        (Lit (apply f args))
        (apply residualize-apply f-name args))))

(define (prim-env)
  (map (lambda (p) (cons (Prim-name p) (box (list p))))
       (list
	(Prim 'sval-known? (lambda (x) (Lit (sval-known? x))))
	(Prim '+ (lambda vals
                   (define-values (known unknown) (partition sval-known? vals))
                   (define part-val (apply + (map Lit-value known)))
                   (define part (Lit part-val))
                   (cond [(null? unknown) part]
                         [(zero? part-val) (apply residualize-apply '+ unknown)]
                         [else (apply residualize-apply '+ part unknown)])))
	(Prim '- (lift-residualize '- -))
	(Prim '< (lift-residualize '< <))
	(Prim 'cons Cons)
	(Prim 'null? (lift-residualize* 'null? (lambda (x) (and (Lit? x) (null? (Lit-value x))))))
	(Prim 'pair? (lift-residualize* 'pair? (lambda (x)
                                                 (or (Cons? x)
                                                     (and (Lit? x) (pair? (Lit-value x)))))))
	(Prim 'zero? (lift-residualize 'zero? zero?))
	(Prim 'eq? (lambda (x y)
		     (if (and (Lit? x)
			      (Lit? y))
			 (Lit (eq? (Lit-value x) (Lit-value y)))
			 (residualize-apply 'eq? x y))))
	(Prim 'PRIMcar (lambda (x)
                         (match x
                           [(Cons a _) a]
                           [(Lit (cons a _)) (Lit a)]
                           [_ (residualize-apply 'PRIMcar x)])))
	(Prim 'PRIMcdr (lambda (x)
                         (match x
                           [(Cons _ d) d]
                           [(Lit (cons _ d)) (Lit d)]
                           [_ (residualize-apply 'PRIMcdr x)]))))))

(define (basic-env)
  (foldl (lambda (entry env)
           (cons (cons (car entry)
                       (box (list (pe (parse (cadr entry) '()) env '()))))
                 env))
         (prim-env)
         (list
          (list 'car '(lambda (x)
                        #:filter 'unfold
                        (if (pair? x)
                            (PRIMcar x)
                            (error "Not a pair in car" x))))
          (list 'cdr '(lambda (x)
                        #:filter 'unfold
                        (if (pair? x)
                            (PRIMcdr x)
                            (error "Not a pair in cdr" x))))
          ;	 (list 'car 'PRIMcar)
          ;	 (list 'cdr 'PRIMcdr)
          (list 'reverse '(lambda (x)
                            #:filter 'unfold
                            (let loop ((x x) (acc '()))
                              (if (null? x)
                                  acc
                                  (loop (cdr x) (cons (car x) acc))))))
          (list 'fold '(lambda (f acc x)
                         #:filter (if (sval-known? f) 'unfold '(#f #f #f))
                         (let loop ((x x) (acc acc))
                           (if (null? x)
                               acc
                               (loop (cdr x) (f (car x) acc))))))
          (list 'fold-right '(lambda (f acc x)
                               #:filter (if (sval-known? f) 'unfold '(#f #f #f))
                               (let loop ((x x))
                                 (if (null? x)
                                     acc
                                     (let ((head (car x)))
                                       (f head (loop (cdr x))))))))
          (list 'list? '(lambda (xs)
                          (let loop ((xs xs))
                            (if (null? xs)
                                #t
                                (if (pair? xs)
                                    (loop (cdr xs))
                                    #f)))))
          (list 'list-of '(lambda (c)
                            (lambda (xs)
                              (let loop ((xs xs))
                                (if (null? xs)
                                    #t
                                    (if (pair? xs)
                                        (if (c (car xs))
                                            (loop (cdr xs))
                                            #f)
                                        #f))))))
          (list 'any/c '(lambda (x)
                          #:filter 'unfold
                          #t))
          (list 'map '(lambda (f x)
                        #:filter 'unfold
                        (fold-right (lambda (v c)
                                      #:filter 'unfold
                                      (cons (f v) c)) '() x)))
          (list 'append '(lambda (a b)
                           #:filter 'unfold
                           (fold-right cons b a)))
          )))

(define (basic-env/streams)
  (foldl (lambda (entry env)
           (cons (cons (car entry)
                       (box (list (pe (parse (cadr entry) '()) env '()))))
                 env))
         (basic-env)
         (list
          (list 'make-stream '(lambda* (stepper state)
                                (cons 'stream (cons stepper (cons state '())))))
          (list 'stream-stepper '(lambda* (stream)
                                   (PRIMcar (PRIMcdr stream))))
          (list 'stream-state '(lambda* (stream)
                                 (PRIMcar (PRIMcdr (PRIMcdr stream)))))
          (list 'stream-maker '(lambda* (stepper)
                                 (lambda* (state)
                                   (make-stream stepper state))))
          (list 'list-stream-stepper '(lambda (l done skip yield)
                                        (if (null? l)
                                            (done)
                                            (yield (car l) (cdr l)))))
          (list 'list->stream '(stream-maker list-stream-stepper))
          (list 'string->stream '(lambda* (s)
                                   (make-stream (lambda (index done skip yield)
                                                  #:filter '(#f #t #t #t)
                                                  (if (= index (string-length s))
                                                      (done)
                                                      (yield (string-ref s index)
                                                             (+ index 1))))
                                                0)))
          (list 'smap
                '(lambda* (f stream)
                   (let ((stepper (stream-stepper stream)))
                     (make-stream (lambda* (state done skip yield)
                                    (stepper state
                                             done
                                             skip
                                             (lambda* (elt new-state) (yield (f elt) new-state))))
                                  (stream-state stream)))))
          (list 'sfilter '(lambda* (pred stream)
                            (let ((stepper (stream-stepper stream)))
                              (make-stream (lambda* (state done skip yield)
                                             (stepper state
                                                      done
                                                      skip
                                                      (lambda* (elt new-state)
                                                        (if (pred elt)
                                                            (yield elt new-state)
                                                            (skip new-state)))))
                                           (stream-state stream)))))
          (list 'sfoldr
                '(lambda* (kons knil stream)
                   (let ((stepper (stream-stepper stream)))
                     (let loop ((state (stream-state stream)))
                       (stepper state
                                (lambda* () knil)
                                (lambda* (new-state) (loop new-state))
                                (lambda* (elt new-state) (kons elt (loop new-state))))))))
          (list 'sfoldl
                '(lambda* (kons knil stream)
                   (let ((stepper (stream-stepper stream)))
                     (let loop ((knil knil)
                                (state (stream-state stream)))
                       (stepper state
                                (lambda* () knil)
                                (lambda* (new-state) (loop new-state))
                                (lambda* (elt new-state) (loop (kons elt knil) new-state)))))))
          (list 'stream->list '(lambda* (stream)
                                 (sfoldr cons '() stream)))
          (list 'make-szip-state '(lambda* (cell left right)
                                    (cons cell (cons left (cons right '())))))
          (list 'szip-state-cell '(lambda* (s) (PRIMcar s)))
          (list 'szip-state-left '(lambda* (s) (PRIMcar (PRIMcdr s))))
          (list 'szip-state-right '(lambda* (s) (PRIMcar (PRIMcdr (PRIMcdr s)))))
          (list 'szip
                '(lambda* (left right)
                   (let ((left-stepper (stream-stepper left))
                         (right-stepper (stream-stepper right)))
                     (make-stream
                      (lambda (state done skip yield)
                        ;;#:filter (if (sval-known? (szip-state-cell state)) 'unfold '(#f #f #f #f))
                        (let ((cell (szip-state-cell state)))
                          (cond
                            ((null? cell)
                             (right-stepper
                              (szip-state-right state)
                              done
                              (lambda* (new-right)
                                (skip (make-szip-state '() (szip-state-left state) new-right)))
                              (lambda* (elt new-right)
                                (skip (make-szip-state (cons elt '()) (szip-state-left state) new-right)))))
                            (else
                             (left-stepper
                              (szip-state-left state)
                              done
                              (lambda* (new-left)
                                (skip (make-szip-state cell new-left (szip-state-right state))))
                              (lambda* (elt new-left)
                                (yield (cons elt cell)
                                       (make-szip-state '() new-left (szip-state-right state)))))))))
                      (make-szip-state '() (stream-state left) (stream-state right))))))
          (list 'make-sconcatmap-state '(lambda* (fstep fstate rs)
                                          (cons fstep (cons fstate (cons rs '())))))
          (list 'sconcatmap-state-first-stepper '(lambda* (s) (PRIMcar s)))
          (list 'sconcatmap-state-first-state '(lambda* (s) (PRIMcar (PRIMcdr s))))
          (list 'sconcatmap-state-remaining-streams '(lambda* (s) (PRIMcar (PRIMcdr (PRIMcdr s)))))
          (list 'sconcatmap
                '(lambda* (f streams)
                   (let ((remaining-streams-stepper (stream-stepper streams)))
                     (make-stream (lambda (state done skip yield)
                                    (let ((first-stepper (sconcatmap-state-first-stepper state)))
                                      (if first-stepper
                                          (first-stepper
                                           (sconcatmap-state-first-state state)
                                           (lambda* ()
                                             (skip (make-sconcatmap-state
                                                    #f #f
                                                    (sconcatmap-state-remaining-streams state))))
                                           (lambda* (new-first-state)
                                             (skip (make-sconcatmap-state
                                                    first-stepper new-first-state
                                                    (sconcatmap-state-remaining-streams state))))
                                           (lambda* (elt new-first-state)
                                             (yield elt
                                                    (make-sconcatmap-state
                                                     first-stepper new-first-state
                                                     (sconcatmap-state-remaining-streams state)))))
                                          (remaining-streams-stepper
                                           (sconcatmap-state-remaining-streams state)
                                           done
                                           (lambda* (new-remaining-streams)
                                             (skip (make-sconcatmap-state
                                                    #f #f
                                                    new-remaining-streams)))
                                           (lambda* (first new-remaining-streams)
                                             (let ((first-stream (f first)))
                                               (skip (make-sconcatmap-state
                                                      (stream-stepper first-stream)
                                                      (stream-state first-stream)
                                                      new-remaining-streams))))))))
                                  (make-sconcatmap-state #f #f (stream-state streams))))))
          (list 'sconcatenate
                '(lambda* (streams)
                   (sconcatmap (lambda* (stream) stream) streams)))
          )))

(define (test-exp exp)
  (pe (parse exp '()) (basic-env/streams) '()))

(define (test)
  (let ((result (test-exp '(map (lambda (x)
				  #:filter 'unfold
				  (+ x 1))
				(append '(1 2) rest)))))
    (pretty-print 'test-done)
    (pretty-print (codegen result))))

(define (popt exp)
  (pretty-print (codegen (test-exp exp))))

(define even-exp
  '(lambda (x)
     ;; eta-conversion here to stop the letrec from being elided
     ;; because of bug described above.
     (letrec ((odd? (lambda (x1)
		      (if (zero? x1)
			  #f
			  (even? (- x1 1)))))
	      (even? (lambda (x2)
		       #:filter 'unfold
		       (if (zero? x2)
			   #t
			   (odd? (- x2 1))))))
       (even? x))))

(define fib-exp
  '(letrec ((fib (lambda (n)
		   (if (< n 2)
		       n
		       (+ (fib (- n 1))
			  (fib (- n 2)))))))
     (fib arg)))

(define curried-exp
  '((((lambda (a)
	(lambda (b)
	  (lambda (c)
	    (do-something-with a b c))))
      'aa)
     (bb))
    'cc))

(define curried-exp*
  '((((lambda* (a)
	(lambda* (b)
	  (lambda* (c)
	    (do-something-with a b c))))
      'aa)
     (bb))
    'cc))

(define curried-exp2
  '(let ((bv (bb)))
     ((((lambda (a)
	  (lambda (b)
	    (lambda (c)
	      (do-something-with a b c))))
	'aa)
       bv)
      'cc)))

(define curried-cps-exp
  '((lambda (k a) (k (lambda (k b) (k (lambda (k c) (do-something-with k a b c))))))
    (lambda (bf) (bf (lambda (cf) (cf (lambda (x) (begin x))
				      'cc))
		     (bb)))
    'aa))

(define curried-cps-exp*
  '((lambda* (k a) (k (lambda* (k b) (k (lambda* (k c) (do-something-with k a b c))))))
    (lambda* (bf) (bf (lambda* (cf) (cf (lambda* (x) (begin x))
                                        'cc))
                      (bb)))
    'aa))

(define code-duplication-exp
  '(let ((x (f a b c)))
     (let ((y (g x x)))
       (h y y))))

(define filtering-mapping-exp
  '(fold (lambda* (elt acc)
	   (if (even? elt)
	       (cons elt acc)
	       acc))
	 '()
	 (map (lambda* (x) (* x 2)) mylist)))

(define filtering-mapping-exp2
  '(fold (lambda* (elt acc)
	   (if (even? elt)
	       (cons elt acc)
	       acc))
	 '()
	 (map (lambda* (x) (* x 2)) '(1 2 3 4 5))))

(define sfiltering-smapping-exp
  '(stream->list
    (sfilter (lambda* (elt)
	       (even? elt))
	     (smap (lambda* (x)
		     (* x 2))
		   (list->stream mylist)))))

(define sfiltering-smapping-exp1
  '(sfilter (lambda* (elt)
	      (even? elt))
	    (smap (lambda* (x)
		    (* x 2))
		  mystream)))

(define sfiltering-smapping-exp2
  '(stream->list
    (sfilter (lambda* (elt)
	       (even? elt))
	     (smap (lambda* (x)
		     (* x 2))
		   (list->stream '(1 2 3 4 5))))))

(define list-serializer
  '(letrec ((ser (lambda* (x emit k)
		   (if (pair? x)
		       (emit 'open
			     (lambda* (emit)
			       (letrec ((serlist (lambda* (xs emit k)
						   (if (pair? xs)
						       (ser (car x) emit
							    (lambda* (emit)
							      (serlist (cdr xs) emit k)))
						       (k emit)))))
				 (serlist x emit (lambda* (emit)
						   (emit 'close k))))))
		       (if (number? x)
			   (emit x k)
			   (error 'not-supported-in-ser))))))
     (letrec ((collect (lambda* (v k)
			 (cons v (k collect)))))
       (ser '(12 22 32)
	    collect
	    (lambda* (emit) '())))))

;;; Local Variables:
;;; eval: (put 'lambda* 'scheme-indent-function 1)
;;; End:
