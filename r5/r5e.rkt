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

	 as
	 ocase)

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

(define (write-term v port is-write)
  (display "{" port)
  (display (constructor-name (term-constructor v)) port)
  (for-each (lambda (f)
	      (display " " port)
	      (write f port))
	    (vector->list (term-fields v)))
  (display "}" port))

;; Term = (term Constructor VectorOf<Value>)
(struct term (constructor fields)
	#:transparent
	#:property prop:procedure send-series
	#:property prop:custom-write write-term)

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
(struct binding (name) #:transparent
	#:property prop:custom-write
	(lambda (v port is-write)
	  (write (binding-name v) port)))
(struct literal (value) #:transparent
	#:property prop:custom-write
	(lambda (v port is-write)
	  (write (list 'quote (literal-value v)) port)))
(struct destructor (constructor subpatterns) #:transparent
	#:property prop:custom-write
	(lambda (v port is-write)
	  ;; cheap hack.
	  (write (term (destructor-constructor v)
		       (destructor-subpatterns v)) port)))

(struct metaterm (value) #:transparent)
(struct metapattern (pattern) #:transparent)

(define-syntax meta
  (syntax-rules ()
    ((_ (ctor arg ...))
     (metaterm (ctor arg ...)))))

;; The inert object.
(define inert (coterm '()))

(define (make-constructor! language name field-names)
  (let ((c (constructor language name field-names)))
    (set-language-constructors! language (append (language-constructors language)
						 (list c)))
    c))

(define-syntax define-language
  (syntax-rules ()
    ((_ name ctor-definition ...)
     (begin (define name (language 'name '() (make-parameter inert)))
	    (define-constructor name ctor-definition) ...))))

(define-syntax define-constructor
  (syntax-rules ()
    ((_ language (name field-name ...))
     (define name (make-constructor! language 'name (vector-immutable 'field-name ...))))))

(define-syntax define-behaviour
  (syntax-rules ()
    ((_ language object)
     ((language-behaviour language) object))))

(define-syntax extend-behaviour
  (syntax-rules ()
    ((_ language object)
     (extend-behaviour language dummy-super object))
    ((_ language super object)
     (let ((p (language-behaviour language)))
       (let ((super (p)))
	 (p (extend object super)))))))

(define-syntax let-behaviour
  (syntax-rules ()
    ((_ ((language object) ...) body ...)
     (parameterize (((language-behaviour language) object)
		    ...)
       body ...))))

(define-syntax let-extension
  (syntax-rules ()
    ((_ ((language more ...) ...) body ...)
     (let ((p (language-behaviour language))
	   ...)
       (parameterize ((p (build-extension (p) more ...))
		      ...)
	 body ...)))))

(define-syntax build-extension
  (syntax-rules ()
    ((_ super-expr object)
     (extend object super-expr))
    ((_ super-expr super object)
     (let ((super super-expr))
       (extend object super)))))

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
    (let ((behaviour ((language-behaviour (constructor-language (term-constructor r))))))
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
	     (lambda (result)
	       (cond
		((and (term? result)
		      (eq? <maybe> (constructor-language (term-constructor result))))
		 (if (eq? Nothing (term-constructor result))
		     #f
		     (ensure-termish (vector-ref (term-fields result) 0) language)))
		(else
		 ;; TODO: permit coercion of result into <maybe> ?
		 (error 'try-coerce
			"Coercion must return <maybe> instance; got ~v"
			result))))
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

(define (as super self message)
  (send-as message super self))

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

(define-syntax ocase
  (syntax-rules ()
    ((_ test (pattern expr ...) ...)
     ((object (pattern expr ...) ...) test))))

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

(define-language <maybe>
  (Nothing)
  (Just value))

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
  (object self
    ((meta (coerce '<tsil>))
     (ocase self
	    ((Cons a (Nil))      (Just (Snoc (Nil) a)))
	    ((Cons a (Snoc b c)) (Just (Snoc (Cons a b) c)))
	    (_                   (Nothing))))))

(define (t)
  ((object ((Cons a d) d)) '(1 2)))

(define last
  (object ((Snoc d a) a)))

(define butlast
  (object ((Snoc d a) d)))

(define (r)
  ((object ((Snoc d a) d)) '(1 2)))
