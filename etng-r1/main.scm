(require (lib "1.ss" "srfi") ;; lists
	 (lib "4.ss" "srfi") ;; homogeneous-numeric-vectors, u8vector
	 (lib "8.ss" "srfi") ;; receive
	 (lib "9.ss" "srfi") ;; records
	 (lib "13.ss" "srfi") ;; strings
	 (only (lib "list.ss") mergesort)
	 (lib "pretty.ss")
	 (lib "packrat.ss" "json-scheme"))

;; SRFI-31, "A special form rec for recursive evaluation"
(define-syntax rec
  (syntax-rules ()
    ((rec (NAME . VARIABLES) . BODY)
     (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
    ((rec NAME EXPRESSION)
     (letrec ( (NAME EXPRESSION) ) NAME))))

(print-struct #t)
(define previous-inspector (current-inspector))
(current-inspector (make-inspector))

(define-record-type tng-qname
  (make-qname uri localname)
  qname?
  (uri qname-uri)
  (localname qname-localname))

(current-inspector previous-inspector)

(load "node.scm")
(load "expand-qname.scm")
(load "alternaparse.scm")
;;(load "oo.scm")

(define (generic-node-map fn node)
  (let visit ((node node))
    (cond
     ((node? node)
      (fn node
	  (lambda ()
	    (make-node* (node-kind node)
			(map (lambda (field) (list (car field) (visit (cadr field))))
			     (node-fields node))))))
     ((pair? node)
      (cons (visit (car node))
	    (visit (cdr node))))
     (else
      (fn node
	  (lambda ()
	    node))))))

(define (gen-random-string charcount)
  (list->string
   (let gen ((charcount charcount))
     (if (zero? charcount)
	 '()
	 (cons (string-ref "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
			   (random 64))
	       (gen (- charcount 1)))))))

(define (genqname)
  (make-qname "http://eighty-twenty.org/etng/r1/ns/etng/gensyms#"
	      (string->uninterned-symbol (gen-random-string 16))))

(define *debug-mode* '(sequence-phases))

(define (valid-namespace-prefix? x)
  (symbol? x))

(define etng-r1-languages
  `(
    (toplevel-command
     (%or
      (command-define-namespace (prefix ,valid-namespace-prefix?) (uri ,string?))
      (command-define-values (pattern data-pattern) (value core-exp))
      (command-define-object (name ,qname?) (args (%list-of data-pattern)) (body core-exp))
      (command-exp (value core-exp))))

    (core-exp
     (%or
      (core-send (receiver core-exp) (message core-exp))
      (core-object (methods (%list-of core-method)))
      (core-function (methods (%list-of core-method)))
      (core-message (parts (%list-of core-exp)))
      (core-ref (name ,qname?))
      (core-tuple (elements (%list-of core-exp)))
      (core-lit (value #t))
      ))

    (core-method
     (%or
      (core-constant (patterns (%list-of data-pattern)) (body core-exp))
      (core-method (patterns (%list-of data-pattern)) (body core-exp))
      ))

    (data-pattern
     (%or
      (pat-discard)
      (pat-binding (name ,qname?))
      (pat-tuple (elements (%list-of data-pattern)))
      (pat-lit (value #t))))

    (med-exp
     (%or
      med-object
      (med-send (receiver med-exp) (message med-exp))
      (med-tuple (elements (%list-of med-exp)))
      (med-lit (value #t))
      (med-ref (name ,qname?))
      (med-self)
      (med-super)
      ))

    (med-object
     (%or
      (med-discard (k med-exp))
      (med-bind (name ,qname?) (k med-exp))
      (med-litmatch (value #t) (match-k med-exp) (nomatch-k med-exp))
      (med-tuplematch (arity ,integer?) (match-k med-exp) (nomatch-k med-exp))
      (med-extend (over med-exp) (under med-exp))
      ))

    ))

(define (stdin-results)
  (packrat-port-results "<stdin>" (current-input-port)))

(define (debug-mode=? what)
  (and (memq what *debug-mode*) #t))

(define (etng-eval-node ast qname-env)
  (let ((expanded (expand-qnames ast qname-env)))
    (pretty-print (node->list expanded))
    (node-match expanded
      ((command-define-namespace prefix uri)
       (extend-qname-env qname-env prefix uri))
      (else
       qname-env))))

(define (pp clue x . maybe-transformer)
  (pretty-print (list clue 
		      (if (null? maybe-transformer)
			  x
			  ((car maybe-transformer) x))))
  (newline)
  x)

(define (!pp clue x . maybe-transformer)
  x)

(define (etng-repl)
  (let loop ((qname-env (extend-qname-env* `((,(string->symbol "") .
					      "http://eighty-twenty.org/etng/r1/ns/etng#")
					     (#f . ""))
					   '())))
    (display ">>ETNG>> ")
    (flush-output)
    (let ((results (stdin-results)))
      (read-etng results
		 (lambda (sexp next)
		   (pp 'raw-sexp sexp)
		   (display (etng-sexp->string '() sexp)) (newline)
		   (let* ((ast (pp 'ast (etng-sexp-parse sexp qname-env) node->list)))
		     (if (check-language ast 'core-exp etng-r1-languages #f)
			 (display ";; Language check passed")
			 (error "Failed language check")))
		   (newline)
		   (when (and next (not (eq? next results)))
		     (loop qname-env)))
		 (lambda (error-description)
		   (pretty-print error-description)
		   (loop qname-env))))))

;;; Local Variables:
;;; eval: (put 'node-match 'scheme-indent-function 1)
;;; eval: (put 'rec 'scheme-indent-function 1)
;;; End:
