(define (extend-qname-env env prefix uri)
  (cons (cons prefix uri) env))

(define (expand-qnames ast env)

  (define (expand-command env)
    (rec (x ast)
      (node-match ast
	((command-define-values pattern value)
	 (make-node 'command-define-values
		    'pattern ((expand-pattern env) pattern)
		    'value ((expand-core-exp env) value)))
	((command-define-object name args body)
	 (make-node 'command-define-object
		    'name ((expand-qname env) name)
		    'args (map (expand-pattern env) args)
		    'body ((expand-core-exp env) body)))
	((command-exp value)
	 (make-node 'command-exp 'value ((expand-core-exp env) value)))
	(else ast))))

  (define (expand-pattern env)
    (rec (x ast)
      (node-match ast
	((pat-message parts)
	 (make-node 'pat-message 'parts (map x parts)))
	((pat-binding name)
	 (make-node 'pat-binding 'name ((expand-qname env) name)))
	((pat-tuple elements)
	 (make-node 'pat-tuple 'elements (map x elements)))
	((pat-lit value)
	 (if (qname? value)
	     (make-node 'pat-lit 'value ((expand-qname env) value))
	     ast))
	(else ast))))

  (define (expand-core-exp env)
    (rec (x ast)
      (node-match ast
	((core-namespace prefix uri value)
	 (make-node 'core-namespace
		    'prefix prefix
		    'uri uri
		    'value ((expand-core-exp (extend-qname-env env prefix uri)) value)))
	((core-send receiver message)
	 (make-node 'core-send
		    'receiver (x receiver)
		    'message (x message)))
	((core-lazy pattern value body)
	 (make-node 'core-lazy
		    'pattern ((expand-pattern env) pattern)
		    'value (x value)
		    'body (x body)))
	((core-object methods)
	 (make-node 'core-object 'methods (map (expand-method env) methods)))
	((core-function methods)
	 (make-node 'core-function 'methods (map (expand-method env) methods)))
	((core-message parts)
	 (make-node 'core-message 'parts (map x parts)))
	((core-do head tail)
	 (make-node 'core-do 'head (x head) 'tail (x tail)))
	((core-let pattern value body)
	 (make-node 'core-let
		    'pattern ((expand-pattern env) pattern)
		    'value (x value)
		    'body (x body)))
	((core-ref name)
	 (make-node 'core-ref 'name ((expand-qname env) name)))
	((core-tuple elements)
	 (make-node 'core-tuple 'elements (map x elements)))
	((core-lit value)
	 (if (qname? value)
	     (make-node 'core-lit 'value ((expand-qname env) value))
	     ast))
	((core-self)
	 ast))))

  (define (expand-method env)
    (lambda (ast)
      (node-match ast
	((core-constant patterns body)
	 (make-node 'core-constant
		    'patterns (map (expand-pattern env) patterns)
		    'body ((expand-core-exp env) body)))
	((core-method patterns body)
	 (make-node 'core-method
		    'patterns (map (expand-pattern env) patterns)
		    'body ((expand-core-exp env) body))))))

  (define (expand-qname env)
    (lambda (q)
      (let ((prefix (qname-uri q)))
	(if (symbol? prefix)
	    (make-qname (cond
			 ((assq prefix env) => cdr)
			 (else (error "Unknown namespace prefix" q)))
			(qname-localname q))
	    q))))

  ((expand-command env) ast))
