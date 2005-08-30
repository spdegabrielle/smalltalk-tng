(require 'srfi-1)
(load "json-scheme/portable-packrat.scm")

(define (port-results filename p)
  (base-generator->results
   (let ((ateof #f)
	 (pos (top-parse-position filename)))
     (lambda ()
       (if ateof
	   (values pos #f)
	   (let ((x (read-char p)))
	     (if (eof-object? x)
		 (begin
		   (set! ateof #t)
		   (values pos #f))
		 (let ((old-pos pos))
		   (set! pos (update-parse-position pos x))
		   (values old-pos (cons x x))))))))))

(define (string-results filename s)
  (base-generator->results
   (let ((idx 0)
	 (len (string-length s))
	 (pos (top-parse-position filename)))
     (lambda ()
       (if (= idx len)
	   (values pos #f)
	   (let ((x (string-ref s idx))
		 (old-pos pos))
	     (set! pos (update-parse-position pos x))
	     (set! idx (+ idx 1))
	     (values old-pos (cons x x))))))))

(define (parse-result->value error-text result)
  (if (parse-result-successful? result)
      (parse-result-semantic-value result)
      (error error-text
	     (let ((e (parse-result-error result)))
	       (list error-text
		     (parse-position->string (parse-error-position e))
		     (parse-error-expected e)
		     (parse-error-messages e))))))

(define (packrat-regex exp)
  (cond
   ((string? exp) (lambda (starting-results)
		    (let loop ((pos 0) (results starting-results))
		      (if (= pos (string-length exp))
			  (make-result exp results)
			  (if (char=? (parse-results-token-value results)
				      (string-ref exp pos))
			      (loop (+ pos 1) (parse-results-next results))
			      (make-expected-result (parse-results-position starting-results)
						    exp))))))
   ((and (pair? exp)
	 (null? (cdr exp))
	 (string? (car exp))) (let ((chars (string->list (car exp))))
				(lambda (results)
				  (if (memv (parse-results-token-value results) chars)
				      (make-result #t (parse-results-next results))
				      (make-expected-result (parse-results-position results)
							    exp)))))
   ((and (pair? exp)
	 (

(define lex-tng
  (packrat-parser token
		  (token ((white 