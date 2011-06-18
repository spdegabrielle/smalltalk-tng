#lang racket

(provide language?
	 language-name
	 language-constructors

	 constructor?
	 constructor-language
	 constructor-name
	 constructor-arity
	 constructor-field-names

	 term?
	 term-constructor
	 term-fields

	 coterm?

	 extension?

	 meta

	 inert

	 define-language

	 define-behaviour
	 extend-behaviour
	 let-behaviour
	 let-extension

	 object
	 extend

	 term-in

	 <coercion>
	 coerce

	 define-primitive-rewriter

	 send-as)

(define-syntax make-writer
  (syntax-rules ()
    ((_ class-name instance-name-extractor)
     (lambda (v port is-write)
       (display "{" port)
       (display class-name port)
       (display " " port)
       (display (instance-name-extractor v) port)
       (display "}" port)))))

;; TODO: do I need the mutability in the constructors field?
;; Language = (language Symbol ListOf<Constructor> Parameter<Value>)
(struct language (name
		  [constructors #:mutable]
		  behaviour)
	#:transparent
	#:property prop:custom-write (make-writer "language" language-name)
	)

(define (send-to-constructor c . field-values-list)
  (let ((field-values (list->vector field-values-list)))
    (when (not (= (vector-length field-values)
		  (constructor-arity c)))
      (error 'send-to-constructor
	     "Constructor ~v in language ~v has arity ~v; given ~v"
	     (constructor-name c)
	     (language-name (constructor-language c))
	     (constructor-arity c)
	     field-values-list))
    (term c field-values)))

(define (send-series v . messages)
  (foldl send-single-message v messages))

;; Constructor = (constructor Language Symbol VectorOf<Symbol>)
(struct constructor (language name field-names)
	#:transparent
	#:property prop:procedure send-to-constructor
	#:property prop:custom-write (make-writer "constructor" constructor-name)
	)

(define (constructor-arity c)
  (vector-length (constructor-field-names c)))

;; Term = (term Constructor VectorOf<Value>)
(struct term (constructor fields)
	#:transparent
	#:property prop:procedure send-series)

;; Coterm = (coterm ListOf<(cons Pattern Procedure)>)
(struct coterm (clauses)
	#:transparent
	#:property prop:procedure send-series)

;; Value = Term | Coterm | Extension | Any

;; Extension = (extension Value Value)
(struct extension (primary secondary)
	#:transparent
	#:property prop:procedure send-series)

;; Pattern = 'discard
;;         | (binding Symbol)
;;         | (literal Any)
;;         | (destructor Constructor VectorOf<Pattern>)
(struct binding (name) #:transparent)
(struct literal (value) #:transparent)
(struct destructor (constructor subpatterns) #:transparent)

(struct metaterm (value) #:transparent)
(struct metapattern (pattern) #:transparent)

(define-syntax meta
  (syntax-rules ()
    ((_ (ctor arg ...))
     (metaterm (ctor arg ...)))))

;; The inert object.
(define inert (coterm '()))

(define (default-language-behaviour name)
  ;; TODO: the way language behaviour is defined is all screwed up.
  (object (term (object))))

(define (make-constructor! language name field-names)
  (let ((c (constructor language name field-names)))
    (set-language-constructors! language (append (language-constructors language)
						 (list c)))
    c))

(define-syntax define-language
  (syntax-rules ()
    ((_ name ctor-definition ...)
     (begin (define name (language 'name '() (make-parameter (default-language-behaviour 'name))))
	    (define-constructor name ctor-definition) ...))))

(define-syntax define-constructor
  (syntax-rules ()
    ((_ language (name field-name ...))
     (define name (make-constructor! language 'name (vector-immutable 'field-name ...))))))

(define-syntax define-behaviour
  (syntax-rules ()
    ((_ language (pattern expr ...) ...)
     ((language-behaviour language)
      (object (pattern expr ...) ...)))))

(define-syntax extend-behaviour
  (syntax-rules ()
    ((_ language (pattern expr ...) ...)
     (let ((p (language-behaviour language)))
       (p (extend (object (pattern expr ...) ...) (p)))))))

(define-syntax let-behaviour
  (syntax-rules ()
    ((_ language ((pattern expr ...) ...) body ...)
     (parameterize (((language-behaviour language)
		     (object (pattern expr ...) ...)))
       body ...))))

(define-syntax let-extension
  (syntax-rules ()
    ((_ language ((pattern expr ...) ...) body ...)
     (let ((p (language-behaviour language)))
       (parameterize ((p (extend (object (pattern expr ...) ...) (p))))
	 body ...)))))

(define-syntax compile-pattern
  (syntax-rules (meta quote _)
    ((compile-pattern (meta subpat))
     (metapattern (compile-pattern subpat)))
    ((compile-pattern _)
     'discard)
    ((compile-pattern (quote lit))
     (literal 'lit))
    ((compile-pattern (dtor subpat ...))
     (destructor dtor (vector (compile-pattern subpat) ...)))
    ((compile-pattern var)
     (binding 'var))))

(define-syntax compile-expr
  (lambda (stx)
    (define (flatten x)
      (syntax-case x (meta quote _)
	((meta pat) (flatten (syntax pat)))
	(_ '())
	((quote lit) '())
	((dtor pat ...) (apply append (map flatten (syntax->list (syntax (pat ...))))))
	(var (list (syntax var)))))
    (syntax-case stx ()
      ((_ self pattern body)
       (with-syntax (((binding ...) (flatten (syntax pattern))))
	 (syntax (lambda (self binding ...) body)))))))

(define-syntax object
  (syntax-rules ()
    ((_ (pattern expr ...) ...)
     (object dummy-self (pattern expr ...) ...))
    ((_ self (pattern expr ...) ...)
     (coterm (list (cons (compile-pattern pattern)
			 (compile-expr self pattern (begin expr ...)))
		   ...)))))

(define (extend derived base)
  (extension derived base))

(define (lookup message r)
  (cond
   ((extension? r)
    (or (lookup message (extension-primary r))
	(lookup message (extension-secondary r))))
   ((coterm? r)
    (let search ((clauses (coterm-clauses r)))
      (if (null? clauses)
	  #f
	  (let ((pattern (caar clauses))
		(method (cdar clauses))
		(rest (cdr clauses)))
	    (let ((bindings (parameterize ((coercion-cache (make-hash)))
			      (match-pattern message pattern '()))))
	      (if bindings
		  (lambda (self) (apply method self bindings))
		  (search rest)))))))
   ((term? r)
    (let* ((behaviour-factory ((language-behaviour (constructor-language (term-constructor r)))))
	   (behaviour (send-as r behaviour-factory behaviour-factory)))
      (lookup message behaviour)))
   (else
    (error 'lookup "Expected cotermish, got ~v" r))))

(define coercion-cache (make-parameter #f))

;; ListOf<(cons Procedure Procedure)>
(define primitive-rewriters '())

(define (term-in language value)
  (parameterize ((coercion-cache (make-hash)))
    (ensure-termish value language)))

(define (try-coerce language direct-receiver indirect-receiver)
  (send-as/k (metaterm (coerce (language-name language)))
	     direct-receiver
	     indirect-receiver
	     (lambda (result) (ensure-termish result language))
	     (lambda (message receiver) #f)))

(define (ensure-termish v0 language)
  ;; TODO: check for cyclic coercion maybe?
  ;;(pretty-print `(ensure-termish ,v0 ,language))
  (if (hash-has-key? (coercion-cache) v0)
      (hash-ref (coercion-cache) v0)
      (let ((value (let search ((v v0))
		     (cond
		      ((extension? v) (or (search (extension-primary v))
					  (search (extension-secondary v))))
		      ((term? v) (if (eq? (constructor-language (term-constructor v))
					  language)
				     v
				     (try-coerce language v v0)))
		      ((coterm? v) (try-coerce language v v0))
		      (else (let loop ((rewriters primitive-rewriters))
			      (cond
			       ((null? rewriters) #f)
			       (((caar rewriters) v)
				(ensure-termish ((cdar rewriters) v) language))
			       (else (loop (cdr rewriters))))))))))
	(hash-set! (coercion-cache) v0 value)
	value)))

(define (match-pattern message p bindings)
  ;;(pretty-print `(match-pattern ,message ,p ,bindings))
  (if (metaterm? message)
      (and (metapattern? p)
	   (match-pattern (metaterm-value message)
			  (metapattern-pattern p)
			  bindings))
      (cond
       ((eq? p 'discard)
	bindings)
       ((binding? p)
	(cons message bindings))
       ((literal? p)
	(and (equal? message (literal-value p))
	     bindings))
       ((destructor? p)
	(let* ((c (destructor-constructor p))
	       (m (ensure-termish message (constructor-language c))))
	  (and m
	       (eq? (term-constructor m) c)
	       (let ((field-count (constructor-arity c))
		     (fields (term-fields m))
		     (subpatterns (destructor-subpatterns p)))
		 (let match-fields ((i (- field-count 1))
				    (bindings bindings))
		   (if (negative? i)
		       bindings
		       (let ((new-bindings (match-pattern (vector-ref fields i)
							  (vector-ref subpatterns i)
							  bindings)))
			 (and new-bindings
			      (match-fields (- i 1) new-bindings)))))))))
       ((metapattern? p)
	#f))))

(define (send-single-message message receiver)
  (send-as message receiver receiver))

(define (send-as message direct-receiver indirect-receiver)
  (send-as/k message direct-receiver indirect-receiver values does-not-understand))

(define (send-as/k message direct-receiver indirect-receiver sk fk)
  ;;(pretty-print `(send-as/k ,message ,direct-receiver))
  (let ((method (lookup message direct-receiver)))
    (if method
	(sk (method indirect-receiver))
	(fk message direct-receiver))))

(define (does-not-understand message receiver)
  (error 'does-not-understand
	 "~v does not understand ~v"
	 receiver
	 message))

(define (define-primitive-rewriter! predicate converter)
  (set! primitive-rewriters (cons (cons predicate converter) primitive-rewriters)))

(define-syntax define-primitive-rewriter
  (syntax-rules ()
    ((_ (predicate valuename) body ...)
     (define-primitive-rewriter! predicate
       (lambda (valuename) body ...)))))

;;---------------------------------------------------------------------------

(define-language <debugging>
  (debug-name))

(define-language <coercion>
  (coerce language-name))

(define-language <list>
  (Nil)
  (Cons first rest))

(define-language <racket-string>
  (RacketString s))

(define-language <racket-symbol>
  (RacketSymbol s))

(define-primitive-rewriter (null? v)
  (Nil))

(define-primitive-rewriter (pair? v)
  (Cons (car v) (cdr v)))

(define-primitive-rewriter (string? v)
  (RacketString v))

(define-primitive-rewriter (symbol? v)
  (RacketSymbol v))

(define-language <tsil>
  (Snoc butlast last))

(extend-behaviour <list>
  ((Cons a (Nil))      (object ((meta (coerce '<tsil>)) (Snoc (Nil) a))))
  ((Cons a (Snoc b c)) (object ((meta (coerce '<tsil>)) (Snoc (Cons a b) c)))))

(define (t)
  ((object ((Cons a d) d)) '(1 2)))

(define last
  (object ((Snoc d a) a)))

(define butlast
  (object ((Snoc d a) d)))

(define (r)
  ((object ((Snoc d a) d)) '(1 2)))
