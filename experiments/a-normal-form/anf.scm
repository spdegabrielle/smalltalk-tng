;; Conversion of a core-scheme-like language to ANF
;; after Flanagan et al. 1993, "The Essence of Compiling with Continuations"

;; A-Normal Form, from Wikipedia:
;; EXP ::= VAL VAL
;;      |  let VAR = EXP in EXP
;; VAL ::= lambda VAR . EXP
;;      |  VAR

;; We'll extend the core calculus with a lazy construct, which plays
;; the role of scheme's letrec. We replace simple let with a
;; pattern-matching construct, which covers both let and if. We keep
;; the single-argument restriction for now.
;;
;; In terms of data, we keep closures, and extend the language with
;; tuples and integer and symbol literals.

(require (lib "1.ss" "srfi") ;; lists
	 (lib "8.ss" "srfi") ;; receive
	 (lib "9.ss" "srfi") ;; records
	 (lib "pretty.ss")
	 "justlazy.scm"
	 (lib "packrat.ss" "json-scheme"))

(load "node.scm")
(load "parse-etng.scm")

(define *debug-mode* '(sequence-phases))

(define a-normal-form-languages
  `(
    (core-exp
     (%or
      (core-send (receiver core-exp) (message core-exp))
      (core-case (value core-exp) (clauses (%list-of core-case-clause)))
      (core-lazy (bindings (%list-of core-lazy-binding)) (body core-exp))
      (core-object (methods (%list-of core-method)))
      (core-sequence (exps (%list-of core-exp)))
      (core-let (pattern data-pattern) (value core-exp))
      (core-ref (name ,symbol?))
      (core-tuple (elements (%list-of core-exp)))
      (core-lit (value #t))
      ))

    (core-case-clause
     (core-case-clause (pattern data-pattern) (body core-exp)))

    (core-method
     (%or
      (core-constant (pattern data-pattern) (body core-exp))
      (core-method (pattern data-pattern) (body core-exp))
      ))

    (core-lazy-binding
     (core-lazy-binding (name ,symbol?) (value core-exp)))

    (anf-exp
     (%or
      (anf-send (receiver anf-value) (message anf-value))
      (anf-case (value anf-exp) (clauses (%list-of anf-case-clause)))
      (anf-lazy (bindings (%list-of anf-lazy-binding)) (body anf-exp))
      (anf-tuple (elements (%list-of anf-value)))
      anf-value
      ))

    (anf-case-clause
     (anf-case-clause (pattern data-pattern) (body anf-exp)))

    (anf-lazy-binding
     (anf-lazy-binding (name ,symbol?) (value anf-exp)))

    (anf-value
     (%or
      (anf-lambda (formal ,symbol?) (body anf-exp))
      (anf-ref (name ,symbol?))
      (anf-lit (value #t))
      ))

    (data-pattern
     (%or
      (pat-discard)
      (pat-binding (name ,symbol?))
      (pat-tuple (elements (%list-of data-pattern)))
      (pat-lit (value #t))))

    ))

(define (anf-value? a)
  (check-language a 'anf-value a-normal-form-languages #f))

(define core->anf
  (let ()
    (define (gentemp)
      (gensym 'anftmp))

    (define (make-anf-let pattern value body)
      (make-node 'anf-case
		 'value value
		 'clauses (list (make-node 'anf-case-clause
					   'pattern pattern
					   'body body))))

    (define (normalize-term exp)
      (normalize exp values))

    (define (normalize-name exp k)
      (normalize exp
		 (lambda (a)
		   (if (anf-value? a)
		       (k a)
		       (let ((t (gentemp)))
			 (make-anf-let (make-node 'pat-binding 'name t)
				       a
				       (k (make-node 'anf-ref 'name t))))))))

    (define (normalize-name* exps k)
      (if (null? exps)
	  (k '())
	  (normalize-name (car exps)
			  (lambda (e)
			    (normalize-name* (cdr exps)
					     (lambda (es) (k (cons e es))))))))

    (define (remake-constant-method constant constant-body)
      (make-node 'anf-case-clause
		 'pattern (node-get constant 'core-constant 'pattern)
		 'body constant-body))

    (define (remake-normal-method method)
      (make-node 'anf-case-clause
		 'pattern (node-get method 'core-method 'pattern)
		 'body (normalize-term (node-get method 'core-method 'body))))

    (define (normalize exp k)
      (node-match exp
	((core-send receiver message)
	 (normalize-name receiver
			 (lambda (r)
			   (normalize-name message
					   (lambda (m)
					     (k (make-node 'anf-send 'receiver r 'message m)))))))
	((core-case value clauses)
	 (normalize value
		    (lambda (v)
		      (k (make-node 'anf-case
				    'value v
				    'clauses (map (lambda (clause)
						    (node-match clause
						      ((core-case-clause pattern body)
						       (make-node 'anf-case-clause
								  'pattern pattern
								  'body (normalize-term body)))))
						  clauses))))))
	((core-lazy bindings body)
	 (k (make-node 'anf-lazy
		       'bindings (map (lambda (binding)
					(node-match binding
					  ((core-lazy-binding name value)
					   (make-node 'anf-lazy-binding
						      'name name
						      'value (normalize-term value)))))
				      bindings)
		       'body (normalize-term body))))
	((core-object methods)
	 (receive (constants methods)
	     (partition (lambda (n) (node-kind? n 'core-constant)) methods)
	   (normalize-name* (map (node-getter 'core-constant 'body) constants)
			    (lambda (constant-bodies)
			      (let ((formal (gentemp))
				    (method-clauses (append (map remake-constant-method
								 constants
								 constant-bodies)
							    (map remake-normal-method
								 methods))))
				(k (make-node 'anf-lambda
					      'formal formal
					      'body (make-node 'anf-case
							       'value (make-node 'anf-ref
										 'name formal)
							       'clauses method-clauses))))))))
	((core-sequence exps)
	 (let loop ((exps exps))
	   (cond
	    ((null? exps) (error "Need value in sequence"))
	    ((null? (cdr exps)) (normalize (car exps) k))
	    (else
	     (let ((exp (car exps)))
	       (node-match exp
		 ((core-let pattern value)
		  (normalize value
			     (lambda (v)
			       (make-anf-let pattern v (loop (cdr exps))))))
		 (else
		  (normalize-name exp (lambda (dont-care) (loop (cdr exps)))))))))))
	((core-let pattern value)
	 (error "core-let in invalid position" (node->list exp)))
	((core-ref name)
	 (k (make-node 'anf-ref 'name name)))
	((core-tuple elements)
	 (normalize-name* elements
			  (lambda (es)
			    (k (make-node 'anf-tuple 'elements es)))))
	((core-lit value)
	 (k (make-node 'anf-lit 'value value)))))

    normalize-term))

(define (stdin-results)
  (packrat-port-results "<stdin>" (current-input-port)))

(define (debug-mode=? what)
  (and (memq what *debug-mode*) #t))

(define (node-type-error node type)
  (error "Language match error" (node->list node) type))

(define (sequence-phases datum phase-alist)
  (if (null? phase-alist)
      (begin
	(when (debug-mode=? 'sequence-phases)
	  (display ";; Final phase result is ")
	  (write (node->list datum))
	  (newline))
	datum)
      (let* ((entry (car phase-alist))
	     (phase-name (car entry))
	     (phase-prelanguage (cadr entry))
	     (phase-body (caddr entry))
	     (phase-postlanguage (cadddr entry))
	     (rest (cdr phase-alist)))
	(when (debug-mode=? 'sequence-phases)
	  (display ";;--------------------------------------------------")
	  (newline)
	  (display ";; Applying phase \"")
	  (display phase-name)
	  (display "\" to ")
	  (write (node->list datum))
	  (newline))
	(unless (check-language datum phase-prelanguage a-normal-form-languages #f)
	  (error (string-append "Failed precondition for phase \"" phase-name "\"")
		 (node->list datum)))
	(let ((new-datum (phase-body datum)))
	  (unless (check-language new-datum phase-postlanguage a-normal-form-languages #f)
	    (error (string-append "Failed postcondition for phase \"" phase-name "\"")
		   (node->list new-datum)))
	  (sequence-phases new-datum rest)))))

(define (compiler-front-end-phases exp)
  (sequence-phases
   exp
   `(("expansion and a-normalization"		core-exp ,core->anf anf-exp))))

(define (etng-repl)
  (let loop ()
    (display ">>ETNG>> ")
    (flush-output)
    (let ((results (stdin-results)))
      (parse-etng results
		  (lambda (ast next)
		    (if (node? ast)
			(pretty-print (node->list (compiler-front-end-phases ast)))
			(begin
			  (newline)
			  (display ";; No parse result")
			  (newline)))
		    (when (and next (not (eq? next results)))
		      (loop)))
		  (lambda (error-description)
		    (pretty-print error-description)
		    (loop))))))

;;; Local Variables:
;;; eval: (put 'node-match 'scheme-indent-function 1)
;;; End:
