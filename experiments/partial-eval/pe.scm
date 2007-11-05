;; Similar to the partial-evaluator in "Partial Evaluator as a
;; Compiler for Reflective Languages", Asai, Masuhara, Matsuoka and
;; Yonezawa (1995), but without preactions (yet).
;;
;; Copyright (C) 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>

(require (lib "1.ss" "srfi") (lib "9.ss" "srfi") (lib "pretty.ss"))
;;(require 'srfi-1)

(define (make-node kind . vals0)
  (let walk ((vals vals0)
	     (acc '()))
    (cond
     ((null? vals) (cons kind (reverse acc)))
     ((null? (cdr vals)) (error "Bad arg list to make-node" (cons kind vals0)))
     (else (walk (cddr vals)
		 (cons (list (car vals) (cadr vals)) acc))))))

(define (node-kind node) (car node))

(define (node-ref node field)
  (cond
   ((assq field (cdr node)) => cadr)
   (else (error "No such field in node" (list field node)))))

(define (node-kind? node kind)
  (eq? (node-kind node) kind))

(define (make-lit val)
  (make-node 'lit 'val val))

(define (lit-value node)
  (node-ref node 'val))

(define (lit? node)
  (node-kind? node 'lit))

(define (make-undefined)
  (make-lit 'UNDEF))

(define (parse-let is-rec exp env)
  (let ((names (map car (cadr exp)))
	(inits (map cadr (cadr exp)))
	(bodyexp `(begin ,@(cddr exp))))
    (cond
     ((null? names) (parse bodyexp env))
     (is-rec (let* ((new-env (append names env))
		    (p (lambda (x) (parse x new-env))))
	       (make-node 'letrec
			  'names names
			  'inits (map p inits)
			  'body (p bodyexp))))
     (else (parse `((lambda ,names (filter 'unfold) ,bodyexp) ,@inits) env)))))

(define (parse exp env)
  (let walk ((exp exp))
      (cond
       ((symbol? exp) (make-node 'ref 'name exp))
       ((not (pair? exp)) (make-lit exp))
       (else
	(case (car exp)
	  ((quote) (make-lit (cadr exp)))
	  ((lambda) (let* ((has-filter (and (pair? (caddr exp))
					    (eq? (car (caddr exp)) 'filter)))
			   (formals (cadr exp))
			   (filter (and has-filter (cadr (caddr exp))))
			   (body (if has-filter (cdddr exp) (cddr exp)))
			   (newenv (append formals env)))
		      (make-node 'lambda
				 'formals formals
				 'filter (and filter (parse filter newenv))
				 'body (parse `(begin ,@body) newenv))))
	  ((lambda*) (parse `(lambda ,(cadr exp)
			       (filter 'unfold)
			       ,@(cddr exp)) env))
	  ((begin) (cond
		    ((null? (cdr exp)) (make-undefined))
		    ((null? (cddr exp)) (walk (cadr exp)))
		    (else (make-node 'begin 'exprs (map walk (cdr exp))))))
	  ((if) (make-node 'if
			   'test (walk (cadr exp))
			   'true (walk (caddr exp))
			   'false (if (null? (cdddr exp))
				      (make-undefined)
				      (walk (car (cdddr exp))))))
	  ((let) (if (symbol? (cadr exp))
		     (walk `(letrec ((,(cadr exp) (lambda ,(map car (caddr exp))
						    ,@(cdddr exp))))
			      (,(cadr exp) ,@(map cadr (caddr exp)))))
		     (parse-let #f exp env)))
	  ((letrec) (parse-let #t exp env))
	  ((let*) (if (null? (cadr exp))
		      (walk `(begin ,@(cddr exp)))
		      (walk `(let (,(caadr exp)) (let* ,(cdadr exp) ,@(cddr exp))))))
	  (else (make-node 'apply
			   'rator (walk (car exp))
			   'rands (map walk (cdr exp)))))))))

(define (extend-env env names inits)
  (append (if inits
	      (map (lambda (name init)
		     (list name init))
		   names inits)
	      (map (lambda (name)
		     (cons name #f))
		   names))
	  env))

(define (sval-known? node)
  (memq (node-kind node) '(lit cons prim closure)))

(define (WARN message . args)
  (display ";; ")
  (display message)
  (for-each (lambda (x)
	      (display " ")
	      (display x))
	    args)
  (newline))

(define (pe pexp env cache)
  ;;(pretty-print `(pe (pexp ,pexp) ));; (env ,env) (cache ,cache)))
  (case (node-kind pexp)
    ((lit) pexp)
    ((letrec) (let ((newenv (extend-env env (node-ref pexp 'names) #f)))
		(for-each (lambda (name init)
			    (let ((binding (assq name newenv)))
			      (set-cdr! binding (list (pe init newenv cache)))))
			  (node-ref pexp 'names)
			  (node-ref pexp 'inits))
		;; BUG: need to collect free variables of the pe'd body,
		;; and if there are references to any of this pexp's names,
		;; we need to wrap the pe'd body in a letrec of the relevant
		;; names. (Actually, iterate until we've pulled in everything
		;; we need.)
		(pe (node-ref pexp 'body) newenv cache)))
    ((ref) (let* ((name (node-ref pexp 'name))
		  (binding (assq name env)))
	     (cond
	      ((not binding) pexp)
	      ((not (cdr binding)) pexp)
	      (else (cadr binding)))))
    ((lambda) (make-node 'closure
			 'formals (node-ref pexp 'formals)
			 'filter (node-ref pexp 'filter)
			 'body (node-ref pexp 'body)
			 'env env))
    ((begin) (begin
	       (for-each (lambda (exp) (WARN ";; Ignoring" exp))
			 (drop-right (node-ref pexp 'exprs) 1))
	       (pe (car (take-right (node-ref pexp 'exprs) 1)) env cache)))
    ((if) (let ((val (pe (node-ref pexp 'test) env cache)))
	    (if (sval-known? val)
		(if (and (lit? val)
			 (lit-value val))
		    (pe (node-ref pexp 'true) env cache)
		    (pe (node-ref pexp 'false) env cache))
		(make-node 'if
			   'test val
			   'true (pe (node-ref pexp 'true) env cache)
			   'false (pe (node-ref pexp 'false) env cache)))))
    ((apply) (let* ((rator (pe (node-ref pexp 'rator) env cache))
		    (rands (map (lambda (x) (pe x env cache)) (node-ref pexp 'rands))))
	       (if (sval-known? rator)
		   (case (node-kind rator)
		     ((closure)
		      (let* ((filter (node-ref rator 'filter))
			     (formals (node-ref rator 'formals))
			     (rator-env (node-ref rator 'env))
			     (newenv (extend-env rator-env formals rands))
			     (directive (if (not filter)
					    (if (every sval-known? rands)
						'unfold
						(map (lambda (formal) #f) formals))
					    (let ((v (pe filter newenv cache)))
					      (if (not (lit? v))
						  (error "Bad filter" (codegen filter))
						  (lit-value v))))))
			(cond
			 ((eq? directive 'unfold)
			  (pe (node-ref rator 'body)
			      newenv
			      cache))
			 ((and (list? directive)
			       (every boolean? directive))
			  (let* ((prop-rands (map (lambda (prop formal rand)
						    (if prop
							rand
							(make-node 'ref 'name formal)))
						  directive
						  formals
						  rands))
				 (cache-key (cons rator prop-rands))
				 (cache-entry (assoc cache-key cache)))
			    (if cache-entry
				(make-node 'apply-cached
					   'rator (cadr cache-entry)
					   'rands rands)
				(let* ((tmp-sym (gensym))
				       (tmp (make-node 'ref 'name tmp-sym))
				       (lambda-node
					(make-node 'closure
						   'formals formals
						   'filter (node-ref rator 'filter)
						   'body (pe (node-ref rator 'body)
							     (extend-env rator-env
									 formals prop-rands)
							     (cons (list cache-key tmp)
								   cache))
						   'env rator-env)))
				  (make-node 'letrec
					     'names (list tmp-sym)
					     'inits (list lambda-node)
					     'body (make-node 'apply
							      'rator tmp
							      'rands rands))))))
			 (else (error "Bad filter result" directive)))))
		     ((prim)
		      ;;(pretty-print `(prim ,(node-ref rator 'prim-name) ));; ,@rands))
		      (apply (node-ref rator 'handler) rands))
		     (else (error "Bad node kind in rator in apply" (list pexp rator))))
		   (make-node 'apply
			      'rator rator
			      'rands rands))))
    (else (error "Bad node kind in pe" pexp))))

(define (codegen pexp)
  (case (node-kind pexp)
    ((lit) `(quote ,(node-ref pexp 'val)))
    ((cons) `(cons ,(codegen (node-ref pexp 'a)) ,(codegen (node-ref pexp 'd))))
    ((prim) (node-ref pexp 'prim-name))
    ((closure) `(lambda ,(node-ref pexp 'formals)
		  ,@(if (node-ref pexp 'filter)
			`((filter ,(codegen (node-ref pexp 'filter))))
			'())
		  ,(codegen (node-ref pexp 'body))))

    ((letrec) `(letrec ,(map list
			     (node-ref pexp 'names)
			     (map codegen (node-ref pexp 'inits)))
		 ,(codegen (node-ref pexp 'body))))
    ((ref) (node-ref pexp 'name))
    ((lambda) `(lambda ,(node-ref pexp 'formals)
		 ,@(if (node-ref pexp 'filter)
		       `((filter ,(codegen (node-ref pexp 'filter))))
		       '())
		 ,(codegen (node-ref pexp 'body))))
    ((begin) `(begin ,@(map codegen (node-ref pexp 'exprs))))
    ((if) `(if ,(codegen (node-ref pexp 'test))
	       ,(codegen (node-ref pexp 'true))
	       ,(codegen (node-ref pexp 'false))))
    ((apply) `(,(codegen (node-ref pexp 'rator)) ,@(map codegen (node-ref pexp 'rands))))
    ((apply-cached) `(GOTO ,(codegen (node-ref pexp 'rator))
			   ,@(map codegen (node-ref pexp 'rands))))
    (else (error "Bad node-kind in codegen" pexp))))

(define (residualize-apply name . args)
  (make-node 'apply 'rator (make-node 'ref 'name name) 'rands args))

(define (prim-env)
  (map (lambda (entry)
	 (list (car entry)
	       (make-node 'prim
			  'prim-name (car entry)
			  'handler (cadr entry))))
       (list
	(list 'sval-known? (lambda (x)
			     (make-lit (sval-known? x))))
	(list '+ (lambda vals
		   (call-with-values (lambda () (partition sval-known? vals))
		     (lambda (known unknown)
		       (let* ((part-val (apply + (map lit-value known)))
			      (part (make-lit part-val)))
			 (cond
			  ((null? unknown) part)
			  ((zero? part-val) (apply residualize-apply '+ unknown))
			  (else (apply residualize-apply '+ part unknown))))))))
	(list '- (lambda (a b)
		   (if (and (sval-known? a) (sval-known? b))
		       (make-lit (- (lit-value a) (lit-value b)))
		       (residualize-apply '- a b))))
	(list '< (lambda (a b)
		   (if (and (sval-known? a) (sval-known? b))
		       (make-lit (< (lit-value a) (lit-value b)))
		       (residualize-apply '< a b))))
	(list 'cons (lambda (a d) (make-node 'cons 'a a 'd d)))
	(list 'null? (lambda (x)
		       (if (sval-known? x)
			   (make-lit (and (lit? x)
					  (null? (lit-value x))))
			   (residualize-apply 'null? x))))
	(list 'pair? (lambda (x)
		       (if (sval-known? x)
			   (make-lit (or (node-kind? x 'cons)
					 (and (lit? x)
					      (pair? (lit-value x)))))
			   (residualize-apply 'pair? x))))
	(list 'zero? (lambda (x)
		       (if (sval-known? x)
			   (make-lit (zero? (lit-value x)))
			   (residualize-apply 'zero? x))))
	(list 'PRIMcar (lambda (x)
			 (if (sval-known? x)
			     (cond
			      ((node-kind? x 'cons) (node-ref x 'a))
			      ((and (lit? x) (pair? (lit-value x))) (make-lit (car (lit-value x))))
			      (else (residualize-apply 'PRIMcar x)))
			     (residualize-apply 'PRIMcar x))))
	(list 'PRIMcdr (lambda (x)
			 (if (sval-known? x)
			     (cond
			      ((node-kind? x 'cons) (node-ref x 'd))
			      ((and (lit? x) (pair? (lit-value x))) (make-lit (cdr (lit-value x))))
			      (else (residualize-apply 'PRIMcdr x)))
			     (residualize-apply 'PRIMcdr x)))))))

(define (basic-env)
  (fold (lambda (entry env)
	  (cons (list (car entry)
		      (pe (parse (cadr entry) '()) env '()))
		env))
	(prim-env)
	(list
	 (list 'car '(lambda (x)
		       (filter 'unfold)
		       (if (pair? x)
			   (PRIMcar x)
			   (error "Not a pair in car" x))))
	 (list 'cdr '(lambda (x)
		       (filter 'unfold)
		       (if (pair? x)
			   (PRIMcdr x)
			   (error "Not a pair in cdr" x))))
;	 (list 'car 'PRIMcar)
;	 (list 'cdr 'PRIMcdr)
	 (list 'reverse '(lambda (x)
			   (filter 'unfold)
			   (let loop ((x x) (acc '()))
			     (if (null? x)
				 acc
				 (loop (cdr x) (cons (car x) acc))))))
	 (list 'fold '(lambda (f acc x)
			(filter (if (sval-known? f) 'unfold '(#f #f #f)))
			(let loop ((x x) (acc acc))
			  (if (null? x)
			      acc
			      (loop (cdr x) (f (car x) acc))))))
	 (list 'fold-right '(lambda (f acc x)
			      (filter (if (sval-known? f) 'unfold '(#f #f #f)))
			      (let loop ((x x))
				(if (null? x)
				    acc
				    (let ((head (car x)))
				      (f head (loop (cdr x))))))))
	 (list 'map '(lambda (f x)
		       (filter 'unfold)
		       (fold-right (lambda (v c)
				     (filter 'unfold)
				     (cons (f v) c)) '() x)))
	 (list 'append '(lambda (a b)
			  (filter 'unfold)
			  (fold-right cons b a)))
	 )))

(define (test-exp exp)
  (pe (parse exp '()) (basic-env) '()))

(define (test)
  (let ((result (test-exp '(map (lambda (x)
				  (filter 'unfold)
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
		       (filter 'unfold)
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

(define curried-cps-exp
  '((lambda (k a) (k (lambda (k b) (k (lambda (k c) (do-something-with k a b c))))))
    (lambda (bf) (bf (lambda (cf) (cf (lambda (x) (begin x))
				      'cc))
		     (bb)))
    'aa))
