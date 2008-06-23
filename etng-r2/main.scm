(require (lib "1.ss" "srfi") ;; lists
	 (lib "4.ss" "srfi") ;; homogeneous-numeric-vectors, u8vector
	 (lib "8.ss" "srfi") ;; receive
	 (lib "9.ss" "srfi") ;; records
	 (lib "13.ss" "srfi") ;; strings
	 (only (lib "list.ss") mergesort)
	 (lib "pretty.ss"))

(print-struct #t)
(define previous-inspector (current-inspector))
(current-inspector (make-inspector))

(define-record-type tng-qname
  (make-qname uri localname)
  qname?
  (uri qname-uri)
  (localname qname-localname))

(current-inspector previous-inspector)

(load "../../ometa-scheme/ometa.scm")

(define etng-naked-id-terminators (string->list "`.()[]{}:;,'\""))

(define (char-etng-id-alpha? ch)
  (or (char-alphabetic? ch)
      (eqv? ch #\_)))

(define (char-etng-id-punct? ch)
  (not (or (char-alphabetic? ch)
	   (char-whitespace? ch)
	   (char-numeric? ch)
	   (memv ch etng-naked-id-terminators))))

(define (eol-char? c)
  (or (eqv? c #\return)
      (eqv? c #\newline)))

(define EMPTY-SYMBOL (string->symbol ""))
(define QUOTE-QNAME (make-qname EMPTY-SYMBOL 'quote))
(define UNQUOTE-QNAME (make-qname EMPTY-SYMBOL 'unquote))
(define SEMI (string->symbol ";"))
(define COMMA (string->symbol ","))
(define ARROW '->)
(define DISCARD '_)

(define (list-interleave x xs)
  (cond
   ((null? xs) '())
   ((null? (cdr xs)) xs)
   (else (cons (car xs) (cons x (list-interleave x (cdr xs)))))))

(define (invert-sign x) (- x))

(define (etng-sexp-special-match? sexps qname)
  (and (pair? sexps)
       (let ((tok (car sexps)))
	 (equal? tok qname))))

(define (special-segment-head? token)
  (or (equal? token QUOTE-QNAME)
      (equal? token UNQUOTE-QNAME)
      (memq token '(namespace do let))))

(define read-etng* (load-ometa "etng-reader.g"))
(define parse-etng* (load-ometa "etng-parser.g"))

(define (read-etng input ks kf)
  (read-etng* 'sexp input ks kf))

(define (read-etng-toplevel input ks kf)
  (read-etng* 'sexp-toplevel input ks kf))

(define (pp clue x . maybe-transformer)
  (pretty-print (list clue 
		      (if (null? maybe-transformer)
			  x
			  ((car maybe-transformer) x))))
  (newline)
  x)

(define (!pp clue x . maybe-transformer)
  x)

(define (etng-repl)
  (let loop ((input (current-input-stream)))
    (display ">>ETNG>> ")
    (flush-output)
    (read-etng-toplevel
     input
     (lambda (sexp next err)
       (pp 'raw-sexp sexp)
       (newline)
       (parse-etng* 'toplevel (list (cons 'paren sexp))
		    (lambda (ast dummy-next err)
		      (if (null? (input-stream->list dummy-next))
			  (pp 'ast ast)
			  (pp 'parse-err2 err)))
		    (lambda (err)
		      (pp 'parse-err1 err)))
       (when (and next (not (eq? next input)))
	 (loop next)))
     (lambda (error-description)
       (pretty-print error-description)
       (loop (current-input-stream))))))
