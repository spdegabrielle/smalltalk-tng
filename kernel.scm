(require 'srfi-1)
(require 'oo)

;---------------------------------------------------------------------------

(define (describe-object! o description)
    (for-each (lambda (entry)
		(let* ((delegating? (vector? entry))
		       (entry (if delegating? (vector->list entry) entry))
		       (name (car entry))
		       (value (cadr entry))
		       (kind (if (null? (cddr entry)) 'immutable (caddr entry))))
		  (add-slot! o name value delegating? kind)))
	      description)
    o)

(define (make-basic-object description)
  (let ((o (make-object* (make-layout*) (vector))))
    (describe-object! o description)))

(define (make-named-object name description)
  (let ((o (make-basic-object description)))
    (add-slot! o 'name name #f 'immutable)))

(define (make-method* selector formal-names body-exp)
  (let ((m (clone-object *method*)))
    (set-slot! m 'selector selector)
    (set-slot! m 'arguments formal-names)
    (set-slot! m 'code body-exp)
    m))

(define (build-getter-body name)
  (lambda (method self)
    (get-slot self name)))

(define (build-setter-body name)
  (lambda (method self value)
    (set-slot! self name value)))

(define (make-getter-method-for name)
  (let ((m (make-method* name '#(self) (build-getter-body name))))
    (set-slot! m 'accessor name)
    m))

(define (make-setter-method-for name mutator-name)
  (let ((m (make-method* mutator-name '#(self value) (build-setter-body name))))
    (set-slot! m 'accessor name)
    m))

(define (make-traits name description)
  (let ((t (make-named-object name `(#(traits ,*traits-traits*)))))
    (describe-object! t description)
    t))

(define primitive-traits-hook
  (make-parameter
   (lambda (o)
     (error "Cannot compute primitive traits" o))))

(define (traits-for-primitive o)
  (cond
   ((number? o) *traits-number*)
   ((char? o) *traits-character*)
   ((symbol? o) *traits-symbol*)
   ((vector? o) *traits-tuple*)
   ((pair? o) *traits-pair*)
   ((string? o) *traits-string*)
   ((boolean? o) *traits-boolean*)
   ((null? o) *nil*)
   (else ((primitive-traits-hook) o))))

;---------------------------------------------------------------------------

(define *image-root* 'uninitialised-image-root)

(let-syntax ((def (syntax-rules () ((_ (v n) ...) (begin (define v '(uninitialised v)) ...)))))
  (include "root-hooks.scm"))

(define (compute-roots-globals)
  (let-syntax ((def (syntax-rules ()
			 ((_ (v n) ...)
			  (filter car (list (list 'n v) ...))))))
    (include "root-hooks.scm")))

(define *primitive-table* 'uninitialised-primitive-table)

(define (store-primitive! key body-procedure)
  (cond
   ((assoc key *primitive-table*) =>
    (lambda (cell)
      (debug 0 "Warning: replacing primitive: "key)
      (set-cdr! cell body-procedure)))
   (else
    (set! *primitive-table* (cons (cons key body-procedure) *primitive-table*)))))

(define (lookup-primitive key)
  (cond
   ((assoc key *primitive-table*) => cdr)
   (else (error "Missing primitive" key))))

(define (reset-primitive-table!)
  (set! *primitive-table* '())
  (let-syntax ((define-method
		 (lambda (x)
		   (syntax-case x ()
		     ((_ (selector arg ...) primitive-name body ...)
		      (with-syntax ((((name role) ...)
				     (map (syntax-rules ()
					    ((_ (name role)) (name role))
					    ((_ name) (name *no-role*)))
					  (syntax ((x arg) ...))))
				    (resend (datum->syntax-object (syntax selector) 'resend)))
			(syntax
			 (begin
			   (debug 1 "Define-method (primitive-side) "'primitive-name
				  " "'selector" "'(role ...))
			   (store-primitive! 'primitive-name
					     (lambda (current-method name ...)
					       (let ((resend (lambda ()
							       (debug 2 "Resending "'selector)
							       (send/previous-method current-method
										     'selector
										     (vector
										      name ...)))))
						 body ...)))))))))))
    (include "kernel-methods.scm")))

(define global-store-hooks '())
(define global-load-hooks '())

(define (store-globals-to-image!)
  (set! *image-root* (make-hash-table eq?))
  (let-syntax ((def (syntax-rules ()
		      ((_ (v n) ...)
		       (begin (hash-table-set! *image-root* 'v v) ...)))))
    (include "root-hooks.scm"))
  (run-hooks! global-store-hooks))

(define (load-globals-from-image!)
  (let-syntax ((def (syntax-rules ()
		      ((_ (v n) ...)
		       (begin (set! v (hash-table-ref *image-root* 'v)) ...)))))
    (include "root-hooks.scm"))
  (run-hooks! global-load-hooks))

;---------------------------------------------------------------------------

(define *root-literals* 'uninitialised-root-literals)
(let-syntax ((def-root-literals (syntax-rules ()
				  ((_ (lit ob) ...)
				   (set! *root-literals*
				     `((,lit ,(lambda () ob)) ...))))))
  (def-root-literals
    (() *nil*)
    (#t *true*)
    (#f *false*)))

(define bootstrap-hooks '())

(define (bootstrap-image!)
  (set! *nil* (make-basic-object `()))
  (set! *no-role* (make-basic-object `()))

  (set! *traits-method* (make-basic-object `()))
  (set! *method* (make-basic-object `()))

  (let ((m *method*))
    (add-slot! m 'traits *traits-method* #t 'immutable #f)
    (add-slot! m 'code *nil* #f 'immutable #f)
    (add-slot! m 'arguments *nil* #f 'immutable #f)
    (add-slot! m 'accessor *nil* #f 'immutable #f)
    (add-slot! m 'primitive *nil* #f 'immutable #f)
    (add-slot! m 'selector *nil* #f 'immutable #f)
    (add-slot! m 'literals *nil* #f 'immutable #f)

    (add-accessors! m 'traits #t)
    (add-accessors! m 'code #t)
    (add-accessors! m 'arguments #t)
    (add-accessors! m 'accessor #t)
    (add-accessors! m 'primitive #t)
    (add-accessors! m 'selector #t)
    (add-accessors! m 'literals #t)

    (add-slot! *nil* 'name "Nil" #f 'immutable)
    (add-slot! *no-role* 'name "NoRole" #f 'immutable)
    (add-slot! *traits-method* 'name "Method" #f 'immutable))

  (set! *traits-traits* (make-named-object "Traits" `()))

  (set! *traits-root* (make-traits "Root" `()))
  (set! *root* (make-basic-object `(#(traits ,*traits-root*))))
  (describe-object! *traits-traits* `(#(root ,*traits-root*)))
  (describe-object! *traits-method* `(#(traits ,*traits-traits*)))

  (set! *traits-oddball* (make-traits "Oddball" `(#(root ,*traits-root*))))
  (set! *traits-derivable* (make-traits "Derivable" `(#(root ,*traits-root*))))
  (set! *traits-cloneable* (make-traits "Cloneable" `(#(derivable ,*traits-derivable*))))
  (describe-object! *traits-method* `(#(cloneable ,*traits-cloneable*)))

  (set! *oddball* (make-basic-object `(#(traits ,*traits-oddball*))))
  (describe-object! *nil* `(#(traits ,*traits-oddball*)))
  (describe-object! *no-role* `(#(traits ,*traits-oddball*)))

  (set! *derivable* (make-basic-object `(#(traits ,*traits-derivable*))))
  (set! *cloneable* (make-basic-object `(#(traits ,*traits-cloneable*))))

  (set! *traits-number* (make-traits "Number" `(#(derivable ,*traits-derivable*))))
  (set! *traits-character* (make-traits "Character" `(#(oddball ,*traits-oddball*))))
  (set! *traits-boolean* (make-traits "Boolean" `(#(oddball ,*traits-oddball*))))
  (set! *traits-symbol* (make-traits "Symbol" `(#(oddball ,*traits-oddball*))))
  (set! *traits-tuple* (make-traits "Tuple" `(#(cloneable ,*traits-cloneable*))))
  (set! *traits-pair* (make-traits "Pair" `(#(cloneable ,*traits-cloneable*))))
  (set! *traits-string* (make-traits "String" `(#(cloneable ,*traits-cloneable*))))

  (set! *traits-socket* (make-traits "Socket" `(#(derivable ,*traits-derivable*))))
  (set! *traits-sdl-surface* (make-traits "SDL_Surface" `(#(oddball ,*traits-oddball*))))
  (set! *traits-sdl-event* (make-traits "SDL_Event" `(#(oddball ,*traits-oddball*))))
  (set! *traits-ttf-font* (make-traits "TTF_Font" `(#(oddball ,*traits-oddball*))))

  (set! *true* (make-basic-object `(#(traits ,*traits-boolean*))))
  (set! *false* (make-basic-object `(#(traits ,*traits-boolean*))))

  (set! *tuple* '#())
  (set! *string* "")
  (set! *symbol* 'Symbol)
  (set! *number* 0)
  (set! *pair* (cons *nil* *nil*))

  (set! *traits-block* (make-traits "Block" `(#(cloneable ,*traits-cloneable*))))
  (set! *block* (make-basic-object `(#(traits ,*traits-block*)
				     (environment ,*nil*))))

  ;; Language-specific -----------
  (set! *traits-cell* (make-traits "Cell" `(#(cloneable ,*traits-cloneable*))))
  (set! *cell* (make-basic-object `(#(traits ,*traits-cell*)
				    (_pvt_value ,*no-role* mutable)
				    (queue () mutable))))
  (set! *traits-location* (make-traits "Location" `(#(cloneable ,*traits-cloneable*))))
  (set! *location* (make-basic-object `(#(traits ,*traits-location*)
					(continuation ,*nil*)
					(parent ,*nil*)
					;;
					;; Need to delegate to parent,
					;; to get exn handler, globals
					;; etc.
					;;
					(children () mutable)
					(dead ,*false* mutable))))
  (set! *boot-block* (metalevel-eval `(block () ((send "fileIn" ((string "boot.thing")))))))

  (set! *globals* (clone-object *cell*))
  (metalevel-inject-cell-value
   *globals*
   (make-basic-object (compute-roots-globals)))
  ;; -----------------------------

  (run-hooks! bootstrap-hooks)
  (flush-literal-objects-table!)

  (for-each (lambda (entry)
	      (install-object-for-literal! (car entry)
					   ((cadr entry))))
	    *root-literals*)

  (let-syntax ((define-method
		 (lambda (x)
		   (syntax-case x ()
		     ((_ (selector arg ...) primitive-name body ...)
		      (with-syntax ((((name role) ...)
				     (map (syntax-rules ()
					    ((_ (name role)) (name role))
					    ((_ name) (name *no-role*)))
					  (syntax ((x arg) ...)))))
			(syntax
			 (begin
			   (debug 1 "Define-method (image-side) "'primitive-name
				  " "'selector" "'(role ...))
			   (let ((method (define-method! 'selector '(name ...) `(,role ...)
					   (lookup-primitive 'primitive-name))))
			     (set-slot! method 'primitive 'primitive-name))))))))))
    (include "kernel-methods.scm"))
)
