#lang racket/base
;; Based on pe.rkt but straying further afield from the original paper.
;; Started 2018-06-30

(require racket/set)
(require racket/match)
(require racket/pretty)
(require (only-in racket/list drop-right last partition append-map))
(require (only-in racket/struct make-constructor-style-printer))

(define-syntax-rule (define-node-struct N (F ...) extra ...)
  (struct N (F ...) #:transparent extra ...))

(define-node-struct Lit (value))
(define-node-struct Letrec (names inits body))
(define-node-struct Ref (name))
(define-node-struct Lambda (formals filter body))
(define-node-struct If (test true false))
(define-node-struct Apply (rator rands))
(define-node-struct Bind (name init body))
(define-node-struct Closure (formals filter body env)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer (lambda (c) 'Closure)
                                     (lambda (c) (list (Closure-formals c)
                                                       (Closure-filter c)
                                                       (Closure-body c)
                                                       (Closure-env c)))))])
(define-node-struct Prim (name handler))
(define-node-struct Cons (a d))

(define (sval-known? node)
  (or (Lit? node)
      (Cons? node)
      (Prim? node)
      (Closure? node)))

(define (parse exp)
  (let walk ((exp exp))
    (match exp
      [(? symbol?) (Ref exp)]

      [`(quote ,e) (Lit e)]

      [`(lambda (,formals ...) #:filter ,filter ,body-exps ...)
       (Lambda formals
               (parse filter)
               (parse `(begin ,@body-exps)))]
      [`(lambda (,formals ...) ,body-exps ...)
       (Lambda formals
               #f
               (parse `(begin ,@body-exps)))]
      [`(lambda* (,formals ...) ,body-exps ...)
       (Lambda formals
               (Lit 'unfold)
               (parse `(begin ,@body-exps)))]

      [`(begin) (Lit (void))]
      [`(begin ,e) (walk e)]
      [`(begin ,e ,es ...) (walk `(let ((,(gensym 'ignored) ,e)) ,@es))]

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
           (Letrec names (map parse inits) (parse `(begin ,@es))))]

      [`(let* () ,es ...)
       (walk `(begin ,@es))]
      [`(let* ((,name ,init) ,more ...) ,es ...)
       (walk `(let ((,name ,init)) (let* ,more ,@es)))]

      [`(,rator ,rands ...)
       (Apply (walk rator) (map walk rands))]

      [_ (Lit exp)])))

(define *globals* (make-hash))

(define (extend-env/undefined env names)
  (append (for/list [(name names)] (cons name (box #f))) env))

(define (extend-env/ref env names)
  (append (for/list [(name names)] (cons name (box (list (Ref name))))) env))

(define (extend-env env names inits)
  (append (for/list [(name names) (init inits)] (cons name (box (list init)))) env))

(define (lookup-env env name)
  (or (assq name env)
      (and (hash-has-key? *globals* name)
           (cons name (hash-ref *globals* name)))))

(define noisy? #f)

(define (pe pexp env cache)

  (define (->value pexp env cache)
    (match pexp
      [(Lit _) pexp]

      [(Letrec names inits body)
       (define newenv (extend-env/undefined env names))
       (for [(name names) (init inits)]
         (define binding-box (cdr (lookup-env newenv name)))
         (set-box! binding-box (list (->value init newenv cache))))
       ;; BUG: need to collect free variables of the pe'd body, and if
       ;; there are references to any of this pexp's names, we need to
       ;; wrap the pe'd body in a letrec of the relevant names.
       ;; (Actually, iterate until we've pulled in everything we need.)
       (->value body newenv cache)]

      [(Ref name)
       (match (lookup-env env name)
         [#f pexp] ;; TODO: unbound variable error instead?
         [(cons _ (box #f)) pexp] ;; TODO: reference to not-yet-initialized-variable error instead?
         [(cons _ (box (list v))) v])]

      [(Lambda formals filter body)
       (Closure formals filter (->code body (extend-env/ref env formals) cache) env)]

      [(Bind name init body)
       (define value (->value init env cache))
       (->value body (extend-env env (list name) (list value)) cache)]

      [(Cons a d)
       (Cons (->value a env cache)
             (->value d env cache))]

      [(If test true false)
       (define val (->value test env cache))
       (if (sval-known? val)
           (if (and (Lit? val) (Lit-value val))
               (->value true env cache)
               (->value false env cache))
           (If val
               (->value true env cache)
               (->value false env cache)))]

      [(Apply rator0 rands0)
       (eval-many
        ->value
        (cons rator0 rands0)
        env
        cache
        (lambda (rator . rands)
          (if (sval-known? rator)
              (match rator
                [(Closure formals filter body rator-env)
                 (define newenv (extend-env rator-env formals rands))
                 (match (cond [filter (Lit-value (->value filter newenv cache))]
                              [(andmap sval-known? rands) 'unfold]
                              [else (for/list [(f formals)] #f)])
                   ['unfold (->value body newenv cache)]
                   [(list (? boolean? props) ...)
                    (define cache-rands (for/list [(prop props) (formal formals) (rand rands)]
                                          (if prop rand (Ref formal))))
                    (define cache-key (cons rator cache-rands))
                    (match (assoc cache-key cache)
                      [(list _ cached-rator) (Apply cached-rator rands)]
                      [#f
                       (define rec-proc
                         (let ((tmp-sym (gensym 'rec)))
                           (Letrec (list tmp-sym)
                                   (list
                                    (Closure formals
                                             filter
                                             (->value body
                                                      (extend-env rator-env formals cache-rands)
                                                      (cons (list cache-key (Ref tmp-sym)) cache))
                                             rator-env))
                                   (Ref tmp-sym))))
                       (->value body newenv (cons (list cache-key rec-proc) cache))])])]
                [(Prim name handler)
                 (apply handler rands)])
              (Apply rator rands))))]))

  (define (->code pexp env cache)
    (match pexp
      [(Lit _) pexp]

      [(Letrec names inits body)
       (define newenv (extend-env/ref env names))
       (Letrec names
               (for/list [(init inits)] (->code init newenv cache))
               (->code body newenv cache))]

      [(Ref name)
       (match (lookup-env env name)
         [#f pexp] ;; TODO: unbound variable error instead?
         [(cons _ (box #f)) pexp] ;; TODO: reference to not-yet-initialized-variable error instead?
         [(cons _ (box (list v))) v])]

      [(Lambda formals filter body)
       (Lambda formals filter (->code body (extend-env/ref env formals) cache))]

      [(Bind name init body)
       ;; (printf "B ~v ~v ~v\n" name init body)
       (define code (->code init env cache))
       (if (Lambda? code)
           (->code body (extend-env env (list name) (list code)) cache)
           (Bind name code (->code body (extend-env/ref env (list name)) cache)))
       ;; (Bind name (->code init env cache) (->code body (extend-env/ref env (list name)) cache))
       ;; (->code body (extend-env env (list name) (list (->code init env cache))) cache)
       ]

      [(Cons a d)
       (Cons (->code a env cache)
             (->code d env cache))]

      [(If test true false)
       (define val (->value test env cache))
       (if (sval-known? val)
           (if (and (Lit? val) (Lit-value val))
               (->code true env cache)
               (->code false env cache))
           (If (->code test env cache)
               (->code true env cache)
               (->code false env cache)))]

      [(Apply rator0 rands0)
       (eval-many
        ->code
        (cons rator0 rands0)
        env
        cache
        (lambda (rator-code . rands)
          (define rator (->value rator0 env cache))
          (if (sval-known? rator)
              (match rator
                [(Closure formals filter body rator-env)
                 (define newenv (extend-env rator-env formals rands))
                 (match (cond [filter (Lit-value (->value filter newenv cache))]
                              [(andmap sval-known? rands) 'unfold]
                              [else (for/list [(f formals)] #f)])
                   ['unfold (->code body newenv cache)]
                   [(list (? boolean? props) ...)
                    (define cache-rands (for/list [(prop props) (formal formals) (rand rands)]
                                          (if prop rand (Ref formal))))
                    (define cache-key (cons rator cache-rands))
                    (match (assoc cache-key cache)
                      [(list _ cached-rator) (Apply cached-rator rands)]
                      [#f
                       (define rec-proc
                         (let ((tmp-sym (gensym 'rec)))
                           (Letrec (list tmp-sym)
                                   (list
                                    (Closure formals
                                             filter
                                             (->code body
                                                     (extend-env rator-env formals cache-rands)
                                                     (cons (list cache-key (Ref tmp-sym)) cache))
                                             rator-env))
                                   (Ref tmp-sym))))
                       (->code body newenv (cons (list cache-key rec-proc) cache))])])]
                [(Prim name handler)
                 (apply handler rands)])
              (Apply rator-code rands))))]))

  (define (hoist-binds r k)
    (match r
      [(Bind name init body) (Bind name init (hoist-binds body k))]
      [_ (k r)]))

  (define (eval-many E pexps env cache k)
    (let loop ((pexps pexps) (acc-rev '()))
      (if (null? pexps)
          (apply k (reverse acc-rev))
          (let ((pexp (car pexps)))
            (hoist-binds (E pexp env cache)
                         (lambda (r)
                           (if (or (sval-known? r) (Ref? r) (Lambda? r))
                               (loop (cdr pexps) (cons r acc-rev))
                               (let ((n (gensym 'seq)))
                                 (Bind n r (loop (cdr pexps) (cons (Ref n) acc-rev)))))))))))

  (when noisy?
    (local-require racket/trace) (trace ->code ->value))

  (->value pexp env cache))

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
    [(If test true false) (set-union (free-names test) (free-names true) (free-names false))]
    [(Apply rator rands) (apply set-union (free-names rator) (map free-names rands))]
    [(Bind name init body) (set-remove (free-names body) name)]))

(struct cached-code ([use-count #:mutable] temp-var-sym [reduced-expr #:mutable]) #:transparent)

(define (codegen pexp)
  (let walk ((pexp pexp))
    (match pexp
      [(Lit v) `(quote ,v)]

      [(Cons a (Lit '())) `(LIST ,(walk a))]
      [(Cons a d)
       (match (walk d)
         [(cons 'LIST r) `(LIST ,(walk a) ,@r)]
         [dc `(cons ,(walk a) ,dc)])]

      [(Prim name _) name]

      [(Closure formals filter body env)
       `(let (,@(for/list [(fname (free-names pexp))]
                  (match (lookup-env env fname)
                    [#f (list fname '_____)]
                    [(cons _name (box (list v))) (list fname (walk v))])))
          (lambda ,formals
            ,@(if filter
                  `(#:filter ,(walk filter))
                  `())
            ,(walk body)))]

      [(Letrec names inits body)
       `(letrec ,(for/list [(name names) (init inits)] (list name (walk init)))
          ,(walk body))]

      [(Ref name) name]

      [(Lambda formals #f body)
       `(lambda ,formals ,(walk body))]
      [(Lambda formals filter body)
       `(lambda ,formals #:filter ,(walk filter) ,(walk body))]

      [(If test true false) `(if ,(walk test)
                                 ,(walk true)
                                 ,(walk false))]

      [(Apply rator rands) `(,(walk rator) ,@(map walk rands))]
      [(Bind name init body) `(let ((,name ,(walk init))) ,(walk body))]
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

(define (lift-commutative-associative-binop f-name f identity)
  (lambda vals
    (define-values (known unknown) (partition sval-known? vals))
    (define part-val (apply f (map Lit-value known)))
    (define part (Lit part-val))
    (cond [(null? unknown) part]
          [(= identity part-val) (apply residualize-apply f-name unknown)]
          [else (apply residualize-apply f-name part unknown)])))

(for-each (lambda (p) (hash-set! *globals* (Prim-name p) (box (list p))))
          (list
           (Prim 'sval-known? (lambda (x) (Lit (sval-known? x))))
           (Prim '+ (lift-commutative-associative-binop '+ + 0))
           (Prim '* (lift-commutative-associative-binop '* * 1))
           (Prim '- (lift-residualize '- -))
           (Prim '< (lift-residualize '< <))
           (Prim 'cons (lambda (a d)
                         (if (and (Lit? a) (Lit? d))
                             (Lit (cons (Lit-value a) (Lit-value d)))
                             (Cons a d))))
           (Prim 'null? (lift-residualize* 'null? (lambda (x) (and (Lit? x) (null? (Lit-value x))))))
           (Prim 'pair? (lift-residualize* 'pair? (lambda (x)
                                                    (or (Cons? x)
                                                        (and (Lit? x) (pair? (Lit-value x)))))))
           (Prim 'number? (lift-residualize* 'number? (lambda (x) (and (Lit? x) (number? (Lit-value x))))))
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
                              [_ (residualize-apply 'PRIMcdr x)])))))

(define (extend-globals! entry)
  (hash-set! *globals*
             (car entry)
             (box (list (pe (parse (cadr entry)) '() '())))))

(for-each extend-globals!
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
           ))

(for-each extend-globals!
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
           ))

(define (test-exp exp)
  (printf "---------------------------------------------------------------------------\n")
  (pe (parse exp) '() '()))

(require racket/trace) (trace test-exp)

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

(define smapping-exp
  '(smap (lambda* (x)
           (* x 2))
         mystream))

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
                   (cond
                     [(pair? x)
                      (emit 'open
                            (lambda* (emit)
                              (letrec ((serlist (lambda* (xs emit k)
                                                  (if (pair? xs)
                                                      (ser (car xs) emit
                                                           (lambda* (emit)
                                                             (serlist (cdr xs) emit k)))
                                                      (k emit)))))
                                (serlist x emit (lambda* (emit)
                                                  (emit 'close k))))))]
                     [(number? x)
                      (emit x k)]
                     [else
                      (error 'not-supported-in-ser)]))))
     (letrec ((collect (lambda* (v k)
			 (cons v (k collect)))))
       (ser '(12 22 32)
	    collect
	    (lambda* (emit) '())))))

(define nonterminating-exp
  `(letrec ((y (lambda (x) (y x)))) (y z)))

(define nonterminating-exp2
  `(letrec ((y (lambda (x) #:filter '(#f) (y x)))) (y 1)))

;; 2018-07-01 11:53:03 At present, this `popt`s to:
;;
;; '(let ((g (let ((+ +)) (lambda (b) #:filter 'unfold (+ b '1))))
;;        (f (let ((* *)) (lambda (a) #:filter 'unfold (* a '2)))))
;;    (lambda (x) #:filter 'unfold (f (g x))))
;;
;; I would like it to also inline `f` and `g`.
;;
(define compose-exp
  '(let ((compose (lambda* (f g)
                    (lambda* (x)
                      (f (g x))))))
     (compose (lambda* (a) (* a 2))
              (lambda* (b) (+ b 1)))))

(set! noisy? #t)
(require racket/trace) (trace pe)

;;; Local Variables:
;;; eval: (put 'lambda* 'scheme-indent-function 1)
;;; End:
