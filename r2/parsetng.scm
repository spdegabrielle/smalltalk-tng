(require 'srfi-1)
(load "json-scheme/portable-packrat.scm")
(load "../lib/pregexp-20050502/pregexp.scm")

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

(define (packrat-token str)
  (lambda (starting-results)
    (let loop ((pos 0) (results starting-results))
      (if (= pos (string-length str))
	  (make-result str results)
	  (if (and results (char=? (parse-results-token-value results) (string-ref str pos)))
	      (loop (+ pos 1) (parse-results-next results))
	      (make-expected-result (parse-results-position starting-results) str))))))

(define (parse-results-take results n)
  (let loop ((acc '())
	     (results results)
	     (n n))
    (if (zero? n)
	(values (list->string (reverse acc))
		results)
	(loop (cons (parse-results-token-value results) acc)
	      (parse-results-next results)
	      (- n 1)))))

(define (packrat-regex exp)
  (let ((re (if (string? exp) (pregexp exp) exp)))
    (lambda (results)
      (let* ((stream (pregexp-make-stream
		      (lambda (r)
			(if r
			    (cons (parse-results-token-value r)
				  (parse-results-next r))
			    (cons #f #f)))))
	     (match (pregexp-match-head re stream)))
	(if match
	    (let-values (((str next) (parse-results-take results (cdr match))))
	      (make-result str next))
	    (make-expected-result (parse-results-position results) exp))))))
