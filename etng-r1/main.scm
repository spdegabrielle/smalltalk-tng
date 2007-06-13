(require (lib "1.ss" "srfi") ;; lists
	 (lib "4.ss" "srfi") ;; homogeneous-numeric-vectors, u8vector
	 (lib "8.ss" "srfi") ;; receive
	 (lib "9.ss" "srfi") ;; records
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
(load "parse-etng.scm")
(load "oo.scm")

(define *debug-mode* '(sequence-phases))

(define etng-r1-languages
  `(
    (toplevel-command
     (%or
      (command-define-namespace (prefix ,symbol?) (uri ,string?))
      (command-define-values (pattern data-pattern) (value core-exp))
      (command-define-object (name ,qname?) (args (%list-of data-pattern)) (body core-exp))
      (command-exp (value core-exp))))

    (core-exp
     (%or
      (core-namespace (prefix ,symbol?) (uri ,string?) (value core-exp))
      (core-send (receiver core-exp) (message core-exp))
      (core-object (methods (%list-of core-method)))
      (core-function (methods (%list-of core-method)))
      (core-message (parts (%list-of core-exp)))
      (core-do (head core-exp) (tail core-exp))
      (core-let (pattern data-pattern) (value core-exp) (body core-exp))
      (core-ref (name ,qname?))
      (core-tuple (elements (%list-of core-exp)))
      (core-lit (value #t))
      (core-self)
      (core-next-method)
      (core-meta (sexp #t))
      ))

    (core-method
     (%or
      (core-constant (patterns (%list-of data-pattern)) (body core-exp))
      (core-method (patterns (%list-of data-pattern)) (body core-exp))
      ))

    (data-pattern
     (%or
      (pat-discard)
      (pat-message (parts (%list-of data-pattern)))
      (pat-binding (name ,qname?))
      (pat-tuple (elements (%list-of data-pattern)))
      (pat-lit (value #t))))

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

(define (etng-repl)
  (let loop ((qname-env (extend-qname-env '() (string->symbol "")
					  "http://eighty-twenty.org/etng/r1/ns/etng")))
    (display ">>ETNG>> ")
    (flush-output)
    (let ((results (stdin-results)))
      (parse-etng results
		  (lambda (ast next)
		    (let ((new-qname-env
			   (if (node? ast)
			       (if (check-language ast 'toplevel-command etng-r1-languages #f)
				   (etng-eval-node ast qname-env)
				   (begin
				     (newline)
				     (display ";; Failed language check")
				     (newline)
				     (pretty-print (node->list ast))
				     (error "Failed language check")))
			       (begin
				 (newline)
				 (display ";; No parse result")
				 (newline)
				 qname-env))))
		      (when (and next (not (eq? next results)))
			(loop new-qname-env))))
		  (lambda (error-description)
		    (pretty-print error-description)
		    (loop qname-env))))))

;;; Local Variables:
;;; eval: (put 'node-match 'scheme-indent-function 1)
;;; eval: (put 'rec 'scheme-indent-function 1)
;;; End:
