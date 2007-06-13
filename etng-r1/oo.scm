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
  (make-pattern* literals tuples messages binding)
  pattern?
  (literals pattern-literals)
  (tuples pattern-tuples)
  (messages pattern-messages)
  (binding pattern-binding))

;---------------------------------------------------------------------------
; MzScheme magic
(current-inspector previous-inspector)
;---------------------------------------------------------------------------
