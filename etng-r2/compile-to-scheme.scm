(define previous-inspector (current-inspector))
(current-inspector (make-inspector))

(define-record-type etng-function
  (make-etng-function sources clauses)
  etng-function?
  (sources etng-function-sources)
  (clauses etng-function-clauses))

(current-inspector previous-inspector)

;---------------------------------------------------------------------------

(define (mangle-etng-id id)
  (cond
   ((qname? id) (error 'implement-qnames-please))
   ((symbol? id) (string->symbol (string-append "etng___" (symbol->string id))))
   (else (error 'invalid-etng-id id))))

(define (etng-send-via-named-proxy receiver name message)
  (etng-send* receiver (namespace-variable-value (mangle-etng-id name)) message))

(define (etng-send* receiver via message)
  (cond
   ((etng-function? via)
    (let lookup ((clauses (etng-function-clauses via)))
      (if (null? clauses)
	  (error 'does-not-understand receiver via message)
	  ((car clauses)
	   message
	   (lambda (thunk)
	     (thunk receiver))
	   (lambda ()
	     (lookup (cdr clauses)))))))
   ((number? via) (etng-send-via-named-proxy receiver 'numberProxy message))
   ((string? via) (etng-send-via-named-proxy receiver 'stringProxy message))
   (else (error 'illegal-primitive-object receiver via message))))

(define (etng-send receiver message)
  (etng-send* receiver receiver message))

(define (compile-to-scheme ast)

  (define (schemeify tng-sexp)
    (if (pair? tng-sexp)
	(case (car tng-sexp)
	  ((paren) (map schemeify (cdr tng-sexp)))
	  (else (error 'brack-and-brace-illegal-in-scheme-assembly)))
	tng-sexp))

  (define (make-definition id val)
    `(namespace-set-variable-value! ',(mangle-etng-id id) ,val))

  (define (toplevel ast)
    (case (car ast)
      ((define-value) (make-definition (cadr ast) (expr (caddr ast))))
      ((define-function) (make-definition (cadr ast) (expr `(function ,(caddr ast)))))
      (else (expr ast))))

  (define (expr ast)
    (case (car ast)
      ((ref) (mangle-etng-id (cadr ast)))
      ((lit) `',(cadr ast))
      ((object) `(make-etng-function ',(cdr ast) (list ,@(map (method #t) (cdr ast)))))
      ((function) `(make-etng-function ',(cdr ast) (list ,@(map (method #f) (cdr ast)))))
      ((tuple) `(vector ,@(map expr (cdr ast))))
      ((send) `(etng-send ,(expr (cadr ast)) ,(expr (caddr ast))))
      ((assemble) `(let ,(map (lambda (binding)
				`(,(car binding) ,(expr (cadr binding))))
			      (cadr ast))
		     ,(schemeify (cadr (assq 'scheme (caddr ast))))))))

  (define (method should-capture-self)
    (lambda (ast)
      `(lambda (_arg _kt _kf)
	 ,(let* ((patterns (cadr ast))
		 (body (caddr ast))
		 (remaining-patterns (cdr patterns))
		 (continuation (if (null? remaining-patterns)
				   (expr body)
				   (expr `(function (method ,remaining-patterns ,body))))))
	    (define (pattern p on-success on-failure)
	      (case (car p)
		((discard) on-success)
		((bind) `(let ((,(mangle-etng-id (cadr p)) _arg)) ,on-success))
		((lit) `(if (eqv? ',(cadr p) _arg)
			    ,on-success
			    ,on-failure))
		((tuple) `(if (and (vector? _arg)
				   (= (vector-length _arg) ,(length (cdr p))))
			      ,(let match-elts ((elts (cdr p))
						(index 0))
				 (if (null? elts)
				     on-success
				     `(let ((_arg (vector-ref _arg ,index)))
					,(pattern (car elts)
						  (match-elts (cdr elts) (+ index 1))
						  on-failure))))
			      ,on-failure))))
	    (pattern (car patterns)
		     `(_kt (lambda (,(if should-capture-self (mangle-etng-id 'self) '_self))
			     ,continuation))
		     `(_kf))))))

  (toplevel ast))
