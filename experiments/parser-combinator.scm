;; Simple Parser Combinator Library

;;;; UNFINISHED and currently in a more-or-less broken state

(require 'srfi-1)
(require 'srfi-13)

(define-record-type stream-methods
  (make-stream-methods head tail pos)
  stream-methods?
  (head stream-head)
  (tail stream-tail)
  (pos stream-pos))

(define (parser-transform handler)
  (lambda (methods stream sv)
    ;;(print (list 'transform stream sv))
    (values #t stream (handler ((stream-pos methods) stream) sv))))

(define (parser-inject value)
  (parser-transform (lambda (pos sv) value)))

(define (parser-literal pred?)
  (lambda (methods stream sv)
    ;;(print (list 'literal stream sv))
    (let ((token ((stream-head methods) stream)))
      (if (pred? token)
	  (values #t ((stream-tail methods) stream) token)
	  (values #f `(expected ,token) sv)))))

(define (parser-shift handler parser)
  (lambda (methods stream sv)
    (let-values (((success next-or-error sv1) (parser methods stream sv)))
      (if success
	  (values #t next-or-error (handler sv1 sv))
	  (values #f next-or-error sv1)))))

(define (parser-fold kons knil parsers)
  (lambda (methods stream sv0)
    (let loop ((parsers parsers)
	       (stream stream)
	       (sv sv0))
      (if (null? parsers)
	  (values #t stream sv)
	  (let-values (((success next-or-error sv1) ((car parsers) methods stream sv)))
	    (if success
		(loop (cdr parsers) next-or-error (kons sv1 sv))
		(values #f next-or-error sv0)))))))

(define (parser-fold* kons knil . parsers)
  (parser-fold kons knil parsers))

(define (parser-and parsers)
  (parser-fold (lambda (tok sv) tok) #t parsers))

(define (parser-and* . parsers)
  (parser-and parsers))

(define (parser-or parsers)
  (lambda (methods stream sv0)
    (let loop ((parsers parsers)
	       (stream stream)
	       (sv sv0))
      (if (null? parsers)
	  (values #f `(parser-or) sv)
	  (let-values (((success next-or-error sv1) ((car parsers) methods stream sv)))
	    (if success
		(values #t next-or-error sv1)
		(loop (cdr parsers) stream sv)))))))

(define (parser-or* . parsers)
  (parser-or parsers))

(define (parser-repeat minrep maxrep parser)
  (lambda (methods stream sv0)
    (let loop ((count 0)
	       (stream stream)
	       (sv sv0))
      (let-values (((success next-or-error sv1) (parser methods stream sv)))
	(if success
	    (if (and maxrep (>= count maxrep))
		(values #f `(too-many-repeats ,count ,maxrep) sv0)
		(loop (+ count 1) next-or-error sv1))
	    (if (and minrep (>= count minrep))
		(values #t next-or-error sv1)
		(values #f `(too-few-repeats ,count ,minrep) sv0)))))))

(define (scan-string literal-string)
  (let ((chars (string->list literal-string)))
    (parser-and*
     (parser-fold cons
		  '()
		  (map (lambda (ch) (parser-literal (lambda (tok) (eqv? tok ch)))) chars))
     (parser-transform (lambda (pos sv)
			 (print (list 'scan-string sv))
			 (list->string (reverse sv)))))))

(define (string-stream-methods str)
  (let ((len (string-length str)))
    (values (make-stream-methods (lambda (i) (if (>= i len) #f (string-ref str i)))
				 (lambda (i) (if (>= i len) #f (+ i 1)))
				 (lambda (i) i))
	    0)))

(define (build-parser spec)
  (cond
   ((pair? spec)
    (case (car spec)
      ((/) (parser-or (map build-parser (cdr spec))))
      ((seq) (parser-and (map build-parser (cdr spec))))
      ((fold) (parser-fold (cadr spec) (caddr spec) (map build-parser (cdddr spec))))
      ((transform) (parser-transform (cadr spec)))
      ((inject) (parser-inject (cadr spec)))
      ((repeat) (parser-repeat (cadr spec) (caddr spec) (build-parser (cadddr spec))))
      ((+) (parser-repeat 1 #f (build-parser (cadr spec))))
      ((*) (parser-repeat 0 #f (build-parser (cadr spec))))
      (else (error "Invalid parser spec" spec))))
   ((string? spec)
    (scan-string spec))
   ((procedure? spec)
    spec)
   (else (error "Invalid parser spec" spec))))

(define (test)
  (let-values (((m s) (string-stream-methods "goodbye, world")))
    (let ((parser (build-parser `(seq (fold ,cons ()
					    (/ "hello"
					       (fold ,cons () "good" "bye")
					       "goodbye")
					    ", world")
				      (transform ,(lambda (pos sv) (reverse sv)))))))
      (parser m s '()))))
