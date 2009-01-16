(define-syntax define-global!
  (syntax-rules ()
    ((_ 'name value) (define name value))))

(define gensym
  (let ((counter 14641))
    (lambda ()
      (let ((v (string->symbol (string-append "g" (number->string counter)))))
	(set! counter (+ counter 1))
	v))))

(define-global! 'map
  (lambda (f l)
    (if (null? l)
	'()
	(cons (f (car l))
	      (map f (cdr l))))))

(define-global! 'global-env
  (let ()
    (define (prim0 f) (lambda (arguments k) (k (f))))
    (define (prim1 f) (lambda (arguments k) (k (f (car arguments)))))
    (define (prim2 f) (lambda (arguments k) (k (f (car arguments) (cadr arguments)))))
    (define (munge-entry entry) (cons (car entry) (cons (box (cadr entry)) (box (caddr entry)))))
    (map munge-entry
	 `((let macro ,(lambda (x env exp)
			 (let ((names (map car (cadr x)))
			       (inits (map cadr (cadr x)))
			       (exps (cddr x)))
			   (exp `((lambda ,names ,@exps) ,@inits)))))
	   (cond macro ,(lambda (x env exp)
			  (exp (cond
				((null? (cdr x)) `(begin))
				((eq? (caadr x) 'else) `(begin ,@(cdadr x)))
				(else `(if ,(caadr x) (begin ,@(cdadr x)) (cond ,@(cddr x))))))))
	   (case macro ,(lambda (x env exp)
			  (let ((v (gensym)))
			    (exp `(let ((,v ,(cadr x)))
				    (cond
				     ,@(map (lambda (clause)
					      (cond
					       ((eq? (car clause) 'else) clause)
					       ((null? (cdar clause))
						`((eq? ,v ',(caar clause)) ,@(cdr clause)))
					       (else (12345678 'multi-case-not-supported clause))))
					    (cddr x))))))))
	   (and macro ,(lambda (x env exp)
			 (exp (cond
			       ((null? (cdr x)) `(begin))
			       ((null? (cddr x)) (cadr x))
			       (else `(if ,(cadr x) (and ,@(cddr x)) #f))))))
	   (,'quasiquote macro
			 ,(lambda (x env exp)
			    (define (qq exp depth)
			      (cond
			       ((not (pair? exp)) `(quote ,exp))
			       ((eq? (car exp) 'quasiquote)
				`(cons ','quasiquote (cons ,(qq (cadr exp) (+ depth 1)) '())))
			       ((eq? (car exp) 'unquote)
				(if (= depth 1)
				    (cadr exp)
				    `(cons ','unquote (cons ,(qq (cadr exp) (- depth 1)) '()))))
			       ((and (pair? (car exp))
				     (eq? (caar exp) 'unquote-splicing))
				(if (= depth 1)
				    `(append ,(cadar exp) ,(qq (cdr exp) depth))
				    `(cons ,(qq (car exp) (- depth 1))
					   ,(qq (cdr exp) depth))))
			       (else `(cons ,(qq (car exp) depth)
					    ,(qq (cdr exp) depth)))))
			    (exp (qq (cadr x) 1))))
	   (define-global! global ,(prim2
				    (lambda (name value)
				      ;; FIXME: should set if entry already exists!
				      (set! global-env (cons (munge-entry
							      (cons name
								    (cons 'global
									  (cons value '()))))
							     global-env)))))

	   (cdr global ,(prim1 cdr))
	   (cddr global ,(prim1 cddr))
	   (cddar global ,(prim1 cddar))
	   (cdar global ,(prim1 cdar))
	   (cdadr global ,(prim1 cdadr))
	   (car global ,(prim1 car))
	   (cadr global ,(prim1 cadr))
	   (caddr global ,(prim1 caddr))
	   (cadddr global ,(prim1 cadddr))
	   (cadar global ,(prim1 cadar))
	   (caar global ,(prim1 caar))
	   (caadr global ,(prim1 caadr))

	   (box global ,(prim1 box))
	   (unbox global ,(prim1 unbox))
	   (set-box! global ,(prim2 set-box!))
    
	   (append global ,(prim2 append))
	   (reverse global ,(prim1 reverse))
	   (cons global ,(prim2 cons))
	   (eq? global ,(prim2 eq?))
	   (= global ,(prim2 =))
	   (not global ,(prim1 not))
	   (null? global ,(prim1 null?))
	   (pair? global ,(prim1 pair?))
	   (symbol? global ,(prim1 symbol?))

	   (gensym global ,(prim0 gensym))

	   ))))

(define-global! 'make-eval
  (lambda (
	   error
	   undefined
	   allocate-env
	   update-env
	   load-env
	   unbound-variable-read
	   load-literal
	   load-closure
	   do-if
	   push-frame
	   update-frame
	   do-call
	   push-continuation
	   )
    (define (env-null? env) (null? env))
    (define (env-name env) (caar env))
    (define (env-annotation env) (unbox (cadar env)))
    (define (env-value env) (unbox (cddar env)))
    (define (set-env-value! env value)
      (set-box! (cadar env) (update-env (env-name env) (env-annotation env) value))
      (set-box! (cddar env) value))
    (define (env-next env) (cdr env))
    (define (make-env name value next)
      (cons (cons name (cons (box (allocate-env name value)) (box value))) next))
    (define (search-one-env env n k fk)
      (cond
       ((env-null? env) (fk))
       ((eq? (env-name env) n) (k (env-annotation env) (env-value env) env))
       (else (search-one-env (env-next env) n k fk))))
    (define (search-env env n k fk)
      (search-one-env env n k (lambda () (search-one-env global-env n k fk))))
    ;; BUG: not proper expansion-passing-style; need separate expansion phase!
    ;; (how else to implement macrolet?)
    (define (expand x env)
      (if (and (pair? x)
	       (symbol? (car x)))
	  (search-env env (car x)
		      (lambda (annotation v cell) (if (eq? annotation 'macro)
						      (v x env (lambda (exp) (expand exp env)))
						      x))
		      (lambda () x))
	  x))
    (define (make-recursive-env defs env)
      (if (null? defs)
	  env
	  (make-env (caar defs) #f (make-recursive-env (cdr defs) env))))
    (define (e-recursive-definitions defs xs env k)
      (let ((new-env (make-recursive-env defs env)))
	(define (fill-init defs pos)
	  (if (null? defs)
	      (e (cons 'begin xs) new-env k)
	      (e (cdar defs) new-env
		 (push-continuation
		  (lambda (v)
		    (set-env-value! pos v)
		    (fill-init (cdr defs) (env-next pos)))))))
	(fill-init defs new-env)))
    (define (e-body defs xs env k)
      (if (null? xs)
	  (e-recursive-definitions defs xs env k)
	  (let ((x (expand (car xs) env)))
	    (if (not (pair? x))
		(e-recursive-definitions defs (cons x (cdr xs)) env k)
		(case (car x)
		  ((begin) (e-body defs (append (cdr x) (cdr xs)) env k))
		  ((define) (if (pair? (cadr x))
				(e-body (cons (cons (caadr x)
						    `(lambda ,(cdadr x) ,@(cddr x)))
					      defs) (cdr xs) env k)
				(e-body (cons (cons (cadr x) (caddr x)) defs) (cdr xs) env k)))
		  (else (e-recursive-definitions defs (cons x (cdr xs)) env k)))))))
    (define (extend-env-with-actuals formals actuals env)
      (if (null? formals)
	  env
	  (make-env (car formals) (car actuals)
		    (extend-env-with-actuals (cdr formals) (cdr actuals) env))))
    (define (e-operands index unevaluated evaluated env k)
      (if (null? unevaluated)
	  (k (reverse evaluated))
	  (e (car unevaluated) env
	     (push-continuation
	      (lambda (newly-evaluated)
		(e-operands (+ index 1)
			    (cdr unevaluated)
			    (cons (update-frame index newly-evaluated) evaluated)
			    env
			    k))))))
    (define (e x env k)
      (let ((x (expand x env)))
	(cond
	 ((symbol? x) (search-env env x
				  (lambda (annotation v cell)
				    (if (eq? annotation 'macro)
					(error 'macro-in-variable-position x)
					(k (load-env x annotation v))))
				  (lambda ()
				    (k (unbound-variable-read x)))))
	 ((not (pair? x)) (k (load-literal x)))
	 (else
	  (case (car x)
	    ((quote) (k (load-literal (cadr x))))
	    ((define) (error 'internal-definition-in-invalid-position x))
	    ((lambda) (k (load-closure
			  (cadr x)
			  (lambda (actuals k)
			    (let ((new-env (extend-env-with-actuals (cadr x) actuals env)))
			      (e-body '() (cddr x) new-env k))))))
	    ((begin) (cond ((null? (cdr x)) (k (undefined)))
			   ((null? (cddr x)) (e (cadr x) env k))
			   (else (e (cadr x) env
				    (push-continuation
				     (lambda (v)
				       (e (cons 'begin (cddr x)) env k)))))))
	    ((if) (e (cadr x) env
		     (push-continuation
		      (lambda (v)
			(do-if v
			       (lambda () (e (caddr x) env k))
			       (lambda () (e (cadddr x) env k)))))))
	    ((set!) (search-env env (cadr x)
				(lambda (annotation v cell)
				  (if (eq? annotation 'macro)
				      (error 'macro-in-variable-position x)
				      (e (caddr x) env
					 (push-continuation
					  (lambda (v)
					    (set-env-value! cell v)
					    (k v))))))
				(lambda () (error 'unbound-variable x))))
	    (else (e-operands 0 (cdr x) '() env
			      (push-frame (length (cdr x))
					  (lambda (operands)
					    (e (car x) env
					       (push-continuation
						(lambda (operator)
						  (do-call operator operands k)))))))))))))
    (lambda (x)
      (e x '() (lambda (v) v)))))

(define-global! 'eval
  (let ()
    (define (error key val) (12345678 'magic-error-procedure key val))
    (define (undefined) 17)
    (define (allocate-env name v) 'local)
    (define (update-env name old-annotation v) old-annotation)
    (define (load-env name annotation v) v)
    (define (unbound-variable-read x) (error 'unbound-variable-read x))
    (define (load-literal x) x)
    (define (load-closure formals f) f)
    (define (do-if v tk fk) (if v (tk) (fk)))
    (define (push-frame count k) k)
    (define (update-frame index v) v)
    (define (do-call operator operands k) (operator operands k))
    (define (push-continuation k) k)
    (make-eval error undefined allocate-env update-env load-env unbound-variable-read
	       load-literal load-closure do-if push-frame update-frame do-call push-continuation)))

(define-global! 'compile
  (lambda (exp)
    (let ((continuation-depth (make-parameter 0)))
      (define (error key val) (12345678 'magic-error-procedure key val))
      (define (undefined) (load-literal 17))
      (define (allocate-env name v)
	(write `(allocate-env ,name ,v)) (newline)
	'local)
      (define (update-env name old-annotation v)
	(write `(update-env ,name ,old-annotation)) (newline)
	old-annotation)
      (define (load-env name annotation v)
	(write `(load-env ,name ,annotation)) (newline)
	v)
      (define (unbound-variable-read name)
	(write `(load-implicit-global ,name)) (newline)
	'implicit-global-value)
      (define (load-literal x)
	(write `(load-literal ,x)) (newline)
	x)
      (define (load-closure formals f)
	(write `(load-closure ,formals)) (newline)
	(parameterize ((continuation-depth 0))
	  (write `(IN================)) (newline)
	  (f formals (lambda (v)
		       (write `(return)) (newline)
		       v))
	  (write `(OUT===============)) (newline)
	  'closure-result))
      (define (do-if v tk fk)
	(write `(do-if ,v)) (newline)
	(write `tk) (newline)
	(tk)
	(write `fk) (newline)
	(fk))
      (define (push-frame count k)
	(write `(push-frame ,count)) (newline)
	k)
      (define (update-frame index v)
	(write `(update-frame ,index ,v)) (newline)
	v)
      (define (do-call operator operands k)
	(write `(do-call ,(if (= (continuation-depth) 0)
			      'tailcall
			      'normalcall) ,operator ,operands))
	(newline)
	(k 'do-call-result))
      (define (push-continuation k)
	;;(write `(push-continuation)) (newline)
	(continuation-depth (+ (continuation-depth) 1))
	(lambda (v)
	  ;;(write `(pop-continuation ,v)) (newline)
	  (continuation-depth (- (continuation-depth) 1))
	  (k v)))
      ((make-eval error undefined allocate-env update-env load-env unbound-variable-read
		  load-literal load-closure do-if push-frame update-frame do-call push-continuation)
       exp))))

(define (syms x)
  (cond
   ((pair? x) (syms (car x)) (syms (cdr x)))
   ((null? x))
   (else (write x) (newline))))

(define (r* repl-eval)
  (display ">>> ")
  (let ((x (read)))
    (if (eof-object? x)
	'done
	(begin (write (repl-eval x))
	       (newline)
	       (r* repl-eval)))))

(define (r) (r* eval))

(eval `(define-global! 'global-env ',global-env))
(r)
