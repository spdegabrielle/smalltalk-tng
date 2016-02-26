#lang racket/base

;; Implementation of Piumarta/Warth-style "Open, Reusable Object
;; Models" for Racket.

;; Objects: have private state of their own, and a vtable for giving
;; them behaviour.
;;
;; VTables: map method names to closures. Include simple parent-style
;; delegation. Are also objects.

(provide (all-defined-out)) ;; TODO

(define-values (prop:vtable vtable? vtable-getter)
  (make-struct-type-property 'vtable))

(struct simple-vtable (methods parent vtable) #:prefab)

(define (vtable-of o)
  (cond
   ((vtable? o) ((vtable-getter o) o))
   ((simple-vtable? o) (or (simple-vtable-vtable o) vtable-vt))
   (else (error 'vtable-of "Cannot compute vtable for ~v" o))))

(define (bind o name)
  (let ((vt (vtable-of o)))
    (if (and (eq? name 'lookup)
	     (eq? vt vtable-vt))
	(vtable-lookup vt name)
	(send vt 'lookup name))))

(define (send o name . args)
  (let ((closure (bind o name)))
    (apply closure o args)))

(define (vtable-lookup self name)
  (hash-ref (simple-vtable-methods self)
	    name
	    (lambda ()
	      (let ((parent (simple-vtable-parent self)))
		(if parent
		    (send parent 'lookup name)
		    (error 'vtable-lookup "No method called ~v" name))))))

(define (vtable-add-method! self name method)
  (hash-set! (simple-vtable-methods self)
	     name
	     method))

(define vtable-vt (simple-vtable (make-hash) #f #f))

(vtable-add-method! vtable-vt 'lookup vtable-lookup)

(vtable-add-method! vtable-vt 'add-method!
		    (lambda (self name method)
		      (hash-set! (simple-vtable-methods self)
				 name
				 method)))

(send vtable-vt 'add-method! 'delegated
      (lambda (self)
	(simple-vtable (make-hash) self (vtable-of self))))

(send vtable-vt 'add-method! 'allocate
      (lambda (self)
        (simple-vtable (make-hash) #f self)))

;;---------------------------------------------------------------------------

(struct object (vtable fields)
  #:transparent
  #:property prop:vtable (lambda (o) (object-vtable o)))

(define object-vt-vt (send vtable-vt 'delegated))

(send object-vt-vt 'add-method! 'allocate
      (lambda (self field-count)
        (object self (make-vector field-count (void)))))

(define object-vt (send object-vt-vt 'allocate))

(send object-vt 'add-method! 'get-field
      (lambda (self field-index)
        (vector-ref (object-fields self) field-index)))

(send object-vt 'add-method! 'set-field!
      (lambda (self field-index new-value)
        (vector-set! (object-fields self) field-index new-value)))

(print-graph #t)
(define o (send object-vt 'allocate 1))
(send o 'set-field! 0 'hello)
(send o 'get-field 0)
o

;; Let's recap.
;;
;;  - simple-vtable-vtable still exists, so that different instances
;;    of simple-vtable can have different behaviours. However, we no
;;    longer have a cycle among Racket values to represent the cycle
;;    in behaviours: instead, when simple-vtable-vtable is #f, the
;;    module-level variable vtable-vt is used instead.
;;
;;  - vtable-vt does not inherit from object-vt. This is unlike the
;;    original Piumarta/Warth system. It makes sense for us to do
;;    things this way because our Racket-level representations are not
;;    as uniform as a pure Piumarta/Warth setup. Our object-vt and
;;    objects are thus *applications* of vtables, rather than being in
;;    a cycle with them, so vtables do not inherit any protocol of
;;    objects. This is getting closer to Smalltalk-like
;;    subclassing-of-nil.
;;
;;  - I had been missing 'allocate protocol on vtable-vt: in the
;;    Piumarta/Warth system, with its uniform representation,
;;    'allocate is punned; that is, it is the same to allocate a
;;    vtable with certain behaviour as it is to allocate an object
;;    with certain behaviour. In our system, with differing Racket
;;    representations of different things, the pun must be
;;    disentangled. Here, vtable-vt is /purely/ used as behaviour /for
;;    vtables/, and never for other kinds of object, so 'allocate on
;;    vtable-vt should allocate /a vtable/. When it comes to
;;    allocating ordinary objects, we need a vtable that knows how to
;;    build them: see object-vt-vt and object-vt for an example! This
;;    is perhaps getting us closer to having smooth interoperation
;;    between different object layouts and/or different heap layouts
;;    within a single program.
