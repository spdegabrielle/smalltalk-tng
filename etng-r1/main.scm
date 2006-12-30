(require (lib "1.ss" "srfi") ;; lists
	 (lib "8.ss" "srfi") ;; receive
	 (lib "9.ss" "srfi") ;; records
	 (lib "pretty.ss")
	 (lib "packrat.ss" "json-scheme"))

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
(load "parse-etng.scm")

(define *debug-mode* '(sequence-phases))

(define a-normal-form-languages
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
      (core-lazy (pattern data-pattern) (value core-exp) (body core-exp))
      (core-object (methods (%list-of core-method)))
      (core-function (methods (%list-of core-method)))
      (core-message (parts (%list-of core-exp)))
      (core-do (head core-exp) (tail core-exp))
      (core-let (pattern data-pattern) (value core-exp) (body core-exp))
      (core-ref (name ,qname?))
      (core-tuple (elements (%list-of core-exp)))
      (core-lit (value #t))
      (core-self)
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

(define (etng-repl)
  (let loop ()
    (display ">>ETNG>> ")
    (flush-output)
    (let ((results (stdin-results)))
      (parse-etng results
		  (lambda (ast next)
		    (if (node? ast)
			(pretty-print (node->list ast))
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
