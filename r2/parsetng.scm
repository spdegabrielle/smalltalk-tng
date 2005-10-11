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

(define (parse-results->pregexp-stream results)
  (pregexp-make-stream (lambda (r)
			 (if r
			     (cons (parse-results-token-value r)
				   (parse-results-next r))
			     (cons #f #f)))
		       results))

(define (packrat-regex exp)
  (let ((re (if (string? exp) (pregexp exp) exp)))
    (lambda (results)
      (let* ((stream (parse-results->pregexp-stream results))
	     (match (pregexp-match-head re stream)))
	(if match
	    (let-values (((str next) (parse-results-take results (cdar match))))
	      (make-result str next))
	    (make-expected-result (parse-results-position results) exp))))))

(define (packrat-cache key parser)
  (lambda (results)
    (results->result results key
		     (lambda ()
		       (parser results)))))

(define-syntax define-packrat-cached
  (syntax-rules ()
    ((_ (fnname results) body ...)
     (define fnname
       (packrat-cache 'fnname
		      (letrec ((fnname (lambda (results) body ...)))
			fnname))))
    ((_ fnname exp)
     (define fnname
       (packrat-cache 'fnname exp)))))

(define parse-ThiNG
  (packrat-parser (begin
		    (define-packrat-cached (white results)
		      (if (and-let* ((ch (parse-results-token-value results)))
			    (char-whitespace? ch))
			  (white (parse-results-next results))
			  (comment results)))
		    (define-packrat-cached (comment results)
		      (if (eq? (parse-results-token-value results) #\")
			  (skip-comment-body (parse-results-next results))
			  (make-result 'whitespace results)))
		    (define (skip-comment-body results)
		      (if (eq? (parse-results-token-value results) #\")
			  (white (parse-results-next results))
			  (skip-comment-body (parse-results-next results))))
		    (define-packrat-cached ident
		      (packrat-regex "[a-z][a-zA-Z0-9_]*"))
		    datum)
		  (datum ((s <- seq0 '#f) s))
		  (seq0 ((s <- seq1) s)
			(() '(seq)))
		  (seq1 ((d <- dict white '#\, s <- seq1) `(seq ,d ,@(cdr s)))
			((d <- dict) `(seq ,d)))
		  (dict ((e <- entry white d <- dict) `(dict ,e ,@(cdr d)))
			((e <- entry) `(dict ,e))
			((v <- app) v))
		  (entry ((k <- app white '#\: v <- app) (list k v)))
		  (app ((parts <- app*) (fold (lambda (arg op) `(app ,op ,arg)) (car parts) (cdr parts))))
		  (app* ((v <- simple white vs <- app*) (cons v vs))
			((v <- simple) (list v)))
		  (simple ((white d1 <- simple1) d1))
		  (simple1 (('#\{ d <- datum white '#\}) `(quote ,d))
			   (('#\( d <- datum white '#\)) d)
			   (('#\[ d <- datum white '#\]) `(meta ,d))
			   ((i <- ident) (string->symbol i)))))

(define read-ThiNG
  (lambda ()
    (parse-result->value "While parsing ThiNG"
			 (parse-ThiNG (port-results "stdin" (current-input-port))))))

(define string->ThiNG
  (lambda (s)
    (parse-result->value "While parsing ThiNG"
			 (parse-ThiNG (string-results "<string>" s)))))
