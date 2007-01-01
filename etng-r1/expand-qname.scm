(define (extend-qname-env env prefix uri)
  (cons (cons prefix uri) env))

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

(define (expand-qnames ast env)

  (define (expand-qname q env)
    (let ((prefix (qname-uri q)))
      (if (symbol? prefix)
	  (make-qname (cond
		       ((assq prefix env) => cdr)
		       (else (error "Unknown namespace prefix" q)))
		      (qname-localname q))
	  q)))

  (define (expand-generic env)
    (lambda (node uninterested-k)
      (cond
       ((qname? node) (expand-qname node env))
       ((node? node)
	(node-match node
	  ((core-namespace prefix uri value)
	   (make-node 'core-namespace
		      'prefix prefix
		      'uri uri
		      'value (generic-node-map (expand-generic (extend-qname-env env prefix uri))
					       value)))
	  (else (uninterested-k))))
       (else (uninterested-k)))))

  (generic-node-map (expand-generic env) ast))
