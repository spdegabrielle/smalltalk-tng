(define (extend-qname-env env prefix uri)
  (cons (cons prefix uri) env))

(define (extend-qname-env* prefix-cons-alist base)
  (append prefix-cons-alist base))

(define (expand-qname q env)
  (let ((prefix (qname-uri q)))
    (make-qname (cond
		 ((assv prefix env) => cdr)
		 (else (error "Unknown namespace prefix" q)))
		(qname-localname q))))

(define (expand-qnames sexp env)
  (let expand ((sexp sexp))
    (cond
     ((pair? sexp) (cons (car sexp) (map expand (cdr sexp))))
     ((qname? sexp) (expand-qname sexp env))
     (else sexp))))
