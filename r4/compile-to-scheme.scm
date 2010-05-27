(define previous-inspector (current-inspector))
(current-inspector (make-inspector))

(define-record-type etng-alternation
  (make-etng-alternation clauses)
  etng-alternation?
  (clauses etng-alternation-clauses))

(current-inspector previous-inspector)

;---------------------------------------------------------------------------

(define etng-namespaces '())
(define implicit-etng-namespace #f)

(define builtin-namespace-url "http://www.eighty-twenty.org/etng/r2/builtin#")

(define (set-etng-namespace! prefix url)
  (cond
   ((assq prefix etng-namespaces) =>
    (lambda (cell) (set-box! (cdr cell) url)))
   (else
    (set! etng-namespaces (cons (cons prefix (box url)) etng-namespaces)))))

(set-etng-namespace! '|| builtin-namespace-url)

(define (mangle-etng-id* url localname)
  (string->symbol (string-append "etng___" url (symbol->string localname))))

(define (mangle-etng-id id)
  (cond
   ((qname? id)
    (cond
     ((assq (qname-uri id) etng-namespaces) =>
      (lambda (entry)
	(mangle-etng-id* (unbox (cdr entry)) (qname-localname id))))
     (else
      (error 'unknown-qname-prefix id))))
   ((symbol? id)
    (if implicit-etng-namespace
	(mangle-etng-id* implicit-etng-namespace id)
	(mangle-etng-id* "" id)))
   (else (error 'invalid-etng-id id))))

(define (etng-send-via-named-proxy receiver localname message)
  (etng-send* receiver
	      (namespace-variable-value (mangle-etng-id* builtin-namespace-url localname))
	      message))

(define (etng-alternation->parser a)
  (lambda (input ks kf)
    (let loop ((clauses (etng-alternation-clauses a)))
      (if (null? clauses)
	  (kf)
	  ((car clauses) input ks (lambda () (loop (cdr clauses))))))))

(define (make-parser-invocation first-message)
  (let ((fragments-rev (make-parameter (list first-message))))
    (define (fragment-following f)
      (let search ((candidate #f)
		   (fs (fragments-rev)))
	(cond
	 ((null? fs) (error 'should-not-reach-here 'fragment-following))
	 ((eq? (car fs) f) candidate)
	 (else (search (car fs) (cdr fs))))))
    (define (remaining-input-following f)
      (let search ((acc '())
		   (fs (fragments-rev)))
	(cond
	 ((null? fs) (error 'should-not-reach-here 'fragment-following))
	 ((eq? (car fs) f) acc)
	 (else (search (append (car fs) acc) (cdr fs))))))
    (define (stream-fragment f)
      (let loop ((position f))
	(lambda (op k)
	  (case op
	    ((next)
	     (if (null? position)
		 (let ((next-fragment (fragment-following f)))
		   (if next-fragment
		       ((stream-fragment next-fragment) 'next k)
		       (let ((so-far (fragments-rev)))
			 (lambda (ignored-receiver)
			   (lambda (message)
			     (parameterize ((fragments-rev (cons message so-far)))
			       ((stream-fragment message) 'next k)))))))
		 (k (car position) (loop (cdr position)))))
	    ((rest)
	     (k (append position (remaining-input-following f))))
	    (else
	     (error 'invalid-op op))))))
    (stream-fragment first-message)))

(define (etng-lookup via message)
  (cond
   ((etng-alternation? via)
    ((etng-alternation->parser via)
     (make-parser-invocation message)
     (lambda (rhs-thunk-waiting-for-self remaining-input)
       (remaining-input 'rest
			(lambda (remaining-message)
			  (if (null? remaining-message)
			      rhs-thunk-waiting-for-self
			      (lambda (receiver)
				(etng-send (rhs-thunk-waiting-for-self receiver)
					   remaining-message))))))
     (lambda () #f)))
   ((procedure? via)
    (via message))
   (else 'invalid-via (list via message))))

(define (etng-directly-invokable? x)
  (or (procedure? x) ;; a parser-invocation, (lambda (message) ...)
      (etng-alternation? x) ;; a parser without invocation: see etng-lookup
      ))

(define (etng-send* receiver via message)
  (cond
   ((etng-directly-invokable? via)
    (let ((thunk (or (etng-lookup via message)
		     (error 'does-not-understand receiver via message))))
      (thunk receiver)))
   ((number? via) (etng-send-via-named-proxy receiver 'numberProxy message))
   ((string? via) (etng-send-via-named-proxy receiver 'stringProxy message))
   ((qname-or-symbol? via) (etng-send-via-named-proxy receiver 'symbolProxy message))
   ((vector? via) (etng-send-via-named-proxy receiver 'tupleProxy message))
   ((not via) (etng-send-via-named-proxy receiver 'falseProxy message))
   ((eq? via #t) (etng-send-via-named-proxy receiver 'trueProxy message))
   (else (error 'illegal-primitive-object receiver via message))))

(define (etng-send receiver message)
  (etng-send* receiver receiver message))

(define (etng-merge-functions f1 f2)
  (make-etng-alternation (append (etng-alternation-clauses f1) (etng-alternation-clauses f2))))

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
      ((define-namespace) `(set-etng-namespace! ',(cadr ast) ',(caddr ast)))
      ((declare-default-namespace) `(set! implicit-etng-namespace ',(cadr ast)))
      ((define-value) (make-definition (cadr ast) (expr (caddr ast))))
      ((define-function) (make-definition (cadr ast) (expr `(function ,(caddr ast)))))
      (else (expr ast))))

  (define (expr ast)
    (case (car ast)
      ((ref) (mangle-etng-id (cadr ast)))
      ((lit) `',(cadr ast))
      ((object) `(make-etng-alternation (list ,@(map (method (cadr ast)) (cddr ast)))))
      ((function) `(make-etng-alternation (list ,@(map (method #f) (cdr ast)))))
      ((tuple) `(vector ,@(map expr (cdr ast))))
      ((send) `(etng-send ,(expr (cadr ast)) (list ,@(map expr (cddr ast)))))
      ((assemble) `(let ,(map (lambda (binding)
				`(,(car binding) ,(expr (cadr binding))))
			      (cadr ast))
		     ,(schemeify (cadr (assq 'scheme (caddr ast))))))))

  (define (pattern p on-success on-failure)
    (case (car p)
      ((discard) on-success)
      ((bind) `(let ((,(mangle-etng-id (cadr p)) _arg)) ,on-success))
      ((lit) `(if (equal? ',(cadr p) _arg)
		  ,on-success
		  ,on-failure))
      ((tuple) `(if (and (vector? _arg)
			 (= (vector-length _arg) ,(length (cdr p))))
		    ,(let ((tuple-name (gensym '_argtuple)))
		       `(let ((,tuple-name _arg))
			  ,(let match-elts ((elts (cdr p))
					    (index 0))
			     (if (null? elts)
				 on-success
				 `(let ((_arg (vector-ref ,tuple-name ,index)))
				    ,(pattern (car elts)
					      (match-elts (cdr elts) (+ index 1))
					      on-failure))))))
		    ,on-failure))))

  (define (method self-id)
    (lambda (ast)
      (let ((body (caddr ast)))
	`(lambda (_stream _kt _kf)
	   ,(let loop ((patterns (cadr ast)))
	      `(_stream 'next
			(lambda (_arg _stream)
			  ,(let* ((remaining-patterns (cdr patterns)))
			     (pattern (car patterns)
				      (if (null? remaining-patterns)
					  `(_kt (lambda (,(if self-id
							      (mangle-etng-id self-id)
							      '_self))
						  ,(expr body))
						_stream)
					  (loop remaining-patterns))
				      `(_kf))))))))))

  (toplevel ast))
