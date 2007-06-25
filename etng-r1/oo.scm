;; Representation of values.

;; Values can be:
;;  - fully-expanded qnames, i.e. uri-and-localname pairs
;;  - words ("integers")
;;  - byte-vectors (bases for strings)
;;  - tuples
;;  - messages (?)
;;  - objects
;;  - functions (objects that don't have a self)

;; Patterns can be:
;;  - discard
;;  - messages (?)
;;  - bindings
;;  - tuples
;;  - literals, ie. words, qnames, byte-vectors (?)

;; Objects and functions differ only in whether they bind self for the
;; RHS of their method-bodies or not. Otherwise, they are logically an
;; ordered list of mappings from pattern to closure. Thus an object
;; template has an environment template specifying what features of
;; the lexical environment are to be closed over, and an object itself
;; has an environment vector containing the closed-over values.
;;
;; We represent the ordered list using unordered data structures such
;; as hash tables where doing so is indistinguishable from preserving
;; the full ordering of the member clauses of the object.
;;
;; Note that objects can be constructed from smaller objects by
;; application of the traits operators '/', '+', '-' and '@'.

;; words < qnames < byte-vectors
(define (lit<? a b)
  (cond
   ((number? a)		(cond
			 ((number? b) (< a b))
			 (else #t)))
   ((qname? a)		(cond
			 ((qname? b) (or (string<? (qname-uri a) (qname-uri b))
					 (and (string=? (qname-uri a) (qname-uri b))
					      (string<? (symbol->string (qname-localname a))
							(symbol->string (qname-localname b))))))
			 ((number? b) #f)
			 (else #t)))
   ((string? a)		(cond
			 ((string? b) (string<? a b))
			 (else #f)))))

;---------------------------------------------------------------------------
; MzScheme magic
(print-struct #t)
(define previous-inspector (current-inspector))
(current-inspector (make-inspector))
;---------------------------------------------------------------------------

(define-record-type tng-pattern
  (make-pattern* kind datum)
  pattern?
  (kind pattern-kind)
  (datum pattern-datum))

(define-record-type tng-pattern-closure
  (make-pattern-closure code env saved-self)
  pattern-closure?
  (code pattern-closure-code)
  (env pattern-closure-env)
  (saved-self pattern-closure-saved-self))

;---------------------------------------------------------------------------
; MzScheme magic
(current-inspector previous-inspector)
;---------------------------------------------------------------------------

;; Currying of patterns:
;;
;; Methods: [([Pattern] * Body)]
;; Dispatch tree: 
;;
;; - may be no patterns at all! (error situation)


(define (make-etng-object methods env saved-self)

  (define (prepend-one patterns body accumulated-patterns)
    (if (or (null? accumulated-patterns)
	    (not (aggregatable? patterns %%%%HERE ...

  (let loop ((methods methods)
	     (rev-patterns '()))
    (if (null? methods)
	(reverse rev-patterns)
	(let ((method (car methods)))
	  (loop (cdr methods)
		(node-match method
		  ((core-constant patterns body)
		   (prepend-one patterns
				(make-node 'core-lit
					   'value (error 'need-to-have-evaluated-already body))
				rev-patterns))
		  ((core-method patterns body)
		   (prepend-one patterns
				body
				rev-patterns))))))))

(define (etng-match patterns value sk-outer fk-outer)

  (define (match-tuple pats vals bindings sk fk)
    (let ((tuple-length (vector-length pats)))
      (let tuple-loop ((index 0)
		       (bindings bindings))
	(if (= index tuple-length)
	    (sk bindings)
	    (match-pat (vector-ref pats index)
		       (vector-ref vals index)
		       bindings
		       (lambda (new-bindings) (tuple-loop (+ index 1) new-bindings))
		       fk)))))

  (define (match-pat pat value bindings sk fk)
    (let ((d (pattern-datum pat)))
      (case (pattern-kind pat)
	((literals)
	 (let ((subpat (hash-table-get d value #f)))
	   (if subpat
	       (sk bindings)
	       (fk))))
	((tuples)
	 (if (vector? value)
	     (let ((probe-index (vector-length value)))
	       (if (>= probe-index (vector-length d))
		   (fk)
		   (let ((subpats (vector-ref d probe-index)))
		     (if subpats
			 (match-tuple subpats value bindings sk fk)
			 (fk)))))
	     (fk)))
	((binding)
	 (sk (cons (cons d value) bindings)))
	(else
	 (error 'invalid-etng-match-pattern pat)))))

  (let match-alternatives ((patterns patterns))
    (if (null? patterns)
	(fk)
	(let* ((pat-and-closure (car patterns))
	       (pat (car pat-and-closure))
	       (clo (cdr pat-and-closure))
	       (next-match (lambda () (match-alternatives (cdr patterns)))))
	  (match-pat pat
		     value
		     (lambda (bindings)
		       (sk-outer bindings clo next-match))
		     next-match)))))
