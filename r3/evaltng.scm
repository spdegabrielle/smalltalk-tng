'(tng-cst-grammar
  (<datum> (or (<value>)
	       (tuple <value> ...)))
  (<value> (or (atom <symbol>)
	       (lit <literal>)
	       (adj <value> <value>)
	       (fun (<value> <value>) ...)
	       (eval <value>)
	       (quote <value>)
	       (meta-quote <value>)
	       (discard)))
  (<literal> (or (<integer>))))

(define-record-type tng-promise
  (make-tng-promise* id defined? value)
  tng-promise?
  (id tng-promise-id)
  (defined? tng-promise-defined? set-tng-promise-defined?!)
  (value tng-promise-value set-tng-promise-value!))

(define-record-type tng-closure
  (make-tng-closure clauses outer-env)
  tng-closure?
  (clauses tng-closure-clauses)
  (outer-env tng-closure-outer-env))

(define-record-printer (tng-promise p out)
  (for-each (cut display <> out)
	    (list "#<tng-promise "(tng-promise-id p)" "(tng-promise-defined? p)" "(tng-promise-value p)">")))

(define make-promise-id
  (let ((counter 0))
    (lambda ()
      (let ((val counter))
	(set! counter (+ counter 1))
	val))))

(define-syntax tng
  (syntax-rules ()
    ((_ interp arg ...)
     (make-tng-promise* (make-promise-id) #f (list interp arg ...)))))

(define (force-tng t)
  (if (tng-promise? t)
      (if (tng-promise-defined? t)
	  (tng-promise-value t)
	  (let ((closure (tng-promise-value t)))
	    ;; I am unsure about this recursive force call! Can't things be arranged
	    ;; so that we never promise a promise? %%%
	    (let ((v (force-tng (apply (car closure) (map force-tng (cdr closure))))))
	      (set-tng-promise-defined?! t #t)
	      (set-tng-promise-value! t v)
	      v)))
      t))

(define (eval-error . x) (apply error x))

(define (quote-layer forced-term env)
  (case (car forced-term)
    ((tuple) `(tuple ,@(map (cut eval-ThiNG <> env) (cdr forced-term))))
    ((atom) forced-term)
    ((lit) forced-term)
    ((adj) `(adj ,(eval-ThiNG (cadr forced-term) env)
		 ,(tng quote-layer (caddr forced-term) env)))
    ((fun) (make-tng-closure (cdr forced-term) env))
    ((var quote meta-quote discard)
     (eval-error "quote-layer: pointless quoting" forced-term env))
    (else
     (eval-error "quote-layer: unknown term" forced-term env))))

(define (match-quoted p v b)
  (case (car p)
    ((atom var lit discard) (match-one p v b))
    ((quote) (eval-error "match-quoted: pointless quoting" p v b))
    ((meta-quote) (eval-error "meta-quote unimplemented (in match-quoted)" p v b))
    (else
     (let ((vv (force-tng v)))
       (case (car p)
	 ((tuple) (and (eq? (car vv) 'tuple)
		       (let match-each ((ps (cdr p))
					(vs (cdr vv))
					(b b))
			 (if (null? ps)
			     (and (null? vs) b)
			     (and (not (null? vs))
				  (let ((b1 (match-one (car ps) (car vs) b)))
				    (and b1 (match-each (cdr ps) (cdr vs) b1))))))))
	 ((adj) (and (eq? (car vv) 'adj)
		     (and-let* ((b1 (match-one (cadr p) (cadr vv) b)))
		       (match-quoted (caddr p) (caddr vv) b1))))
	 ;; Variables alternate roles when quoted, and the roles are
	 ;; flipped in pattern context when compared to value context.
	 ;;
	 ;; In a pattern, a variable is a binding occurrence unless
	 ;; it's in a quoted-subpattern, in which case it's a
	 ;; referencing occurrence; In a value, a variable is a
	 ;; referencing occurrence unless it's in a quoted-subvalue,
	 ;; in which case it's a binding occurrence.
	 ;;
	 ;; %%% FIXME: get the scoping right for references in
	 ;; quoted-subpatterns.
	 ;;
	 ((fun) (and (tng-closure? vv)
		     (let ((env b)) ;; see FIXME above
		       (let match-each ((clauses (cdr p))
					(b b))
			 (if (null? clauses)
			     b
			     (let ((pv (caar clauses))
				   (pp (cadar clauses)))
			       (eval-app vv
					 pv
					 env
					 (lambda (code new-env)
					   (let ((result (eval-ThiNG code new-env)))
					     (and-let* ((b1 (match-one pp result b)))
					       (match-each (cdr clauses) b1))))
					 (lambda ()
					   #f))))))))
	 (else (eval-error "match-quoted: unknown term" p vv b)))))))

(define (match-one p v b)
  (case (car p)
    ((var) (cons (cons (cadr p) v) b))
    ((quote) (match-quoted (cadr p) v b))
    ((meta-quote) (eval-error "meta-quote unimplemented (in match-one)" p v b))
    ((discard) b)
    ((adj tuple fun) (eval-error "match-one: missing quoting" p v b))
    (else
     (let ((vv (force-tng v)))
       (case (car p)
	 ((atom) (and (eq? (car vv) 'atom)
		      (eq? (cadr p) (cadr vv))
		      b))
	 ((lit) (and (eq? (car vv) 'lit)
		     (equal? (cadr p) (cadr vv))
		     b))
	 (else (eval-error "match-one: unknown term" p vv b)))))))

(define (match-clause clauses arg outer-env sk fk)
  (let search ((clauses clauses))
    (if (null? clauses)
	(fk)
	(let ((new-env (match-one (caar clauses) arg outer-env)))
	  (if new-env
	      (sk (cadar clauses) new-env)
	      (search (cdr clauses)))))))

(define (eval-app fn arg env sk fk)
  (let ((fn (force-tng fn)))
    (if (tng-closure? fn)
	(let* ((arg (eval-ThiNG arg env)))
	  (match-clause (tng-closure-clauses fn)
			arg
			(tng-closure-outer-env fn)
			sk
			fk))
	(eval-error "eval-app: attempt to apply non-function" fn arg env))))

(define (eval-ThiNG-inner term env)
  (case (car term)
    ((tuple) ;; Parallel evaluation? sigh
     `(tuple ,@(map (cut eval-ThiNG <> env) (cdr term))))
    ((atom) term)
    ((var) (cond
	    ((assq (cadr term) env) => cdr)
	    (else (eval-error "Unbound variable" term env))))
    ((lit) term)
    ((adj) (eval-app (eval-ThiNG (cadr term) env)
		     (caddr term)
		     env
		     eval-ThiNG
		     (lambda ()
		       (eval-error "no match found" term env))))
    ((fun) (eval-error "Situations unimplemented" term env))
    ((quote) (if #f ;; disable quoting through one layer of tupling
		 (let ((v (force-tng (cadr term))))
		   (if (eq? (car v) 'tuple)
		       `(tuple ,@(map (lambda (x) (tng quote-layer x env)) (cdr v)))
		       (quote-layer v env)))
		 (quote-layer (force-tng (cadr term)) env)))
    ((meta-quote) (eval-error "meta-quote unimplemented" term env))
    ((discard) (eval-error "Discard appeared on the right" term env))
    (else (eval-error "Unknown term" term env))))

(define (eval-ThiNG term env)
  (tng eval-ThiNG-inner term env))

(define (call-with-stupid-error-handler f)
  (call-with-current-continuation
   (lambda (escape)
     (fluid-let ((error (lambda x (escape `(ERROR ,@x)))))
       (f)))))

(define (pretty-print-ThiNG x)
  (let ((x (call-with-stupid-error-handler
	    (lambda ()
	      (let walk ((x x))
		(cond
		 ((pair? x) (cons (walk (car x))
				  (walk (cdr x))))
		 ((tng-closure? x) `(fun-closure ,(tng-closure-clauses x)
						 ,(walk (tng-closure-outer-env x))))
		 ((tng-promise? x) (walk (force-tng x)))
		 (else x)))))))
    (pretty-print x)))

(define (repl-ThiNG)
  (display "\":ThiNG:\" ")
  (let ((x (call-with-stupid-error-handler read-ThiNG)))
    (newline)
    (pretty-print x)
    (newline)
    (if (eq? (car x) 'ERROR)
	(repl-ThiNG)
	(if (not (equal? x '(quote (atom quit))))
	    (let ((r x)) ;(call-with-stupid-error-handler (lambda () (eval-ThiNG x '())))))
	      ;;(pretty-print-ThiNG r)
	      (print-tng r 'eval)
	      (newline)
	      (repl-ThiNG))))))

;(trace match-one)
;(trace match-quoted)
;(trace match-clause)
;(trace force-tng)
;(trace eval-app)
;(trace eval-ThiNG-inner)
