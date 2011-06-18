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
	  ((cond) (if (eq? (car (cadr exp)) 'else)
		      (parse `(begin ,@(cdr (cadr exp))) env)
		      (parse `(if ,(car (cadr exp))
				  (begin ,@(cdr (cadr exp)))
				  (cond ,@(cddr exp))) env)))
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
		     (cons name (box (list init))))
		   names inits)
	      (map (lambda (name)
		     (cons name (box #f)))
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
			      (set-box! (cdr binding) (list (pe init newenv cache)))))
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
	      ((not (unbox (cdr binding))) pexp)
	      (else (car (unbox (cdr binding)))))))
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

(define (codegen0 pexp)
  (case (node-kind pexp)
    ((lit) `(quote ,(node-ref pexp 'val)))
    ((cons) (let ((acode (codegen (node-ref pexp 'a)))
		  (d (node-ref pexp 'd)))
	      (if (and (lit? d)
		       (eq? (lit-value d) '()))
		  `(LIST ,acode)
		  (let ((dcode (codegen d)))
		    (if (and (pair? dcode)
			     (eq? (car dcode) 'LIST))
			`(LIST ,acode ,@(cdr dcode))
			`(cons ,acode ,dcode))))))
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

(define (free-names pexp)
  (case (node-kind pexp)
    ((lit) '())
    ((cons) (lset-union eq?
			(free-names (node-ref pexp 'a))
			(free-names (node-ref pexp 'd))))
    ((prim) '())
    ((closure lambda) (lset-difference eq?
				       (free-names (node-ref pexp 'body))
				       (node-ref pexp 'formals)))
    ((letrec) (lset-difference eq?
			       (apply lset-union eq?
				      (free-names (node-ref pexp 'body))
				      (map free-names (node-ref pexp 'inits)))
			       (node-ref pexp 'names)))
    ((ref) (list (node-ref pexp 'name)))
    ((begin) (apply lset-union eq? (map free-names (node-ref pexp 'exprs))))
    ((if) (lset-union eq?
		      (free-names (node-ref pexp 'test))
		      (free-names (node-ref pexp 'true))
		      (free-names (node-ref pexp 'false))))
    ((apply apply-cached) (apply lset-union eq?
				 (free-names (node-ref pexp 'rator))
				 (map free-names (node-ref pexp 'rands))))
    (else (error "Bad node-kind in free-names" pexp))))

(define (codegen pexp)
  (let ((cache (box '()))) ;; alist of (pexp use-count-box temp-var-sym reduced-expr-box)
    (define (walk pexp)
      (cond
       ((assq pexp (unbox cache)) =>
	(lambda (entry)
	  (set-box! (cadr entry) (+ (unbox (cadr entry)) 1))
	  (caddr entry)))
       (else
	(let ((entry (list pexp (box 1) (gensym 't) (box #f))))
	  (set-box! cache (cons entry (unbox cache)))
	  (let ((exp (walk1 pexp)))
	    (set-box! (car (cdddr entry)) exp)
	    (caddr entry))))))
    (define (env-entry-walker names)
      (lambda (entry)
	(if (memq (car entry) names)
	    (list (list (car entry) (walk (car (unbox (cdr entry))))))
	    '())))
    (define (walk1 pexp)
      (case (node-kind pexp)
	((lit) `(quote ,(node-ref pexp 'val)))
	((cons) (let ((acode (walk (node-ref pexp 'a)))
		      (d (node-ref pexp 'd)))
		  (if (and (lit? d)
			   (eq? (lit-value d) '()))
		      `(LIST ,acode)
		      (let ((dcode (walk d)))
			(if (and (pair? dcode)
				 (eq? (car dcode) 'LIST))
			    `(LIST ,acode ,@(cdr dcode))
			    `(cons ,acode ,dcode))))))
	((prim) (node-ref pexp 'prim-name))
	((closure) `(let (,@(append-map (env-entry-walker (free-names (node-ref pexp 'body)))
					(node-ref pexp 'env)))
		      (lambda ,(node-ref pexp 'formals)
			,@(if (node-ref pexp 'filter)
			      `((filter ,(walk (node-ref pexp 'filter))))
			      '())
			,(walk (node-ref pexp 'body)))))
	((letrec) `(letrec ,(map list
				 (node-ref pexp 'names)
				 (map walk (node-ref pexp 'inits)))
		     ,(walk (node-ref pexp 'body))))
	((ref) (node-ref pexp 'name))
	((lambda) `(lambda ,(node-ref pexp 'formals)
		     ,@(if (node-ref pexp 'filter)
			   `((filter ,(walk (node-ref pexp 'filter))))
			   '())
		     ,(walk (node-ref pexp 'body))))
	((begin) `(begin ,@(map walk (node-ref pexp 'exprs))))
	((if) `(if ,(walk (node-ref pexp 'test))
		   ,(walk (node-ref pexp 'true))
		   ,(walk (node-ref pexp 'false))))
	((apply) `(,(walk (node-ref pexp 'rator)) ,@(map walk (node-ref pexp 'rands))))
	((apply-cached) `(GOTO ,(walk (node-ref pexp 'rator))
			       ,@(map walk (node-ref pexp 'rands))))
	(else (error "Bad node-kind in codegen" pexp))))
    (let* ((exp (walk pexp))
	   (remapped-cache (map (lambda (entry)
				  (list (caddr entry) ;; temp-var-sym
					(unbox (cadr entry)) ;; use-count
					(unbox (cadddr entry)))) ;; reduced-expr
				(unbox cache))))
      (define (inlinable? entry)
	(or (= (cadr entry) 1)
	    (let ((v (caddr entry)))
	      (or (symbol? v)
		  (and (pair? v)
		       (pair? (cdr v))
		       (null? (cddr v))
		       (eq? (car v) 'quote)
		       (symbol? (cadr v)))))))
      (define (inline exp)
	(cond
	 ((pair? exp) (cons (inline (car exp)) (inline (cdr exp))))
	 ((assq exp remapped-cache) =>
	  (lambda (entry)
	    (if (inlinable? entry)
		(inline (caddr entry))
		exp)))
	 (else exp)))
      (let ((filtered-mapped-cache (map (lambda (entry)
					  (list (car entry) (cadr entry) (inline (caddr entry))))
					(filter (lambda (entry) (not (inlinable? entry)))
						remapped-cache))))
	`(codegen-result ,(inline exp) ,filtered-mapped-cache)))))

(define (residualize-apply name . args)
  (make-node 'apply 'rator (make-node 'ref 'name name) 'rands args))

(define (prim-env)
  (map (lambda (entry)
	 (cons (car entry)
	       (box (list (make-node 'prim
				     'prim-name (car entry)
				     'handler (cadr entry))))))
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
	(list 'eq? (lambda (x y)
		     (if (and (lit? x)
			      (lit? y))
			 (make-lit (eq? (lit-value x) (lit-value y)))
			 (residualize-apply 'eq? x y))))
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
	  (cons (cons (car entry)
		      (box (list (pe (parse (cadr entry) '()) env '()))))
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
			 (filter 'unfold)
			 #t))
	 (list 'map '(lambda (f x)
		       (filter 'unfold)
		       (fold-right (lambda (v c)
				     (filter 'unfold)
				     (cons (f v) c)) '() x)))
	 (list 'append '(lambda (a b)
			  (filter 'unfold)
			  (fold-right cons b a)))
	 )))

(define (basic-env/streams)
  (fold (lambda (entry env)
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
						 (filter '(#f #t #t #t))
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
		       ;;(filter (if (sval-known? (szip-state-cell state)) 'unfold '(#f #f #f #f)))
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
