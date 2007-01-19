;; Representation of values.

;; Values can be:
;;  - fully-expanded qnames, i.e. uri-and-localname pairs
;;  - words ("integers")
;;  - byte-vectors (bases for strings)
;;  - tuples
;;  - messages (?)
;;  - objects
;;  - functions (objects that don't have a self)
;;  - promises (?)

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
