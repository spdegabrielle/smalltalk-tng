;; A BindingKind is one of
;; -- 'value for ordinary first-class values
;; -- 'language for bindings of second-class Languages
;; -- 'constructor for bindings of second-class Constructors

;; All bindings are lexically scoped.

;; A Binding is a (binding Symbol BindingKind Any)
;; and names the Any by the Symbol. The BindingKind is used during
;; pattern-match and expression compilation to figure out how to parse
;; multi-part templates.
(struct binding (name kind value) #:transparent)

;; A Template is a List<(Either Symbol 

(struct constructor (language-name template

(define (r5eval exp env)
  (