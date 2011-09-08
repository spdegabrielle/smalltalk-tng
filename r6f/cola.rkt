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
   ((simple-vtable? o) (simple-vtable-vtable o))
   (else (error 'vtable-of "Cannot compute vtable for ~v" o))))

(define (bind o name)
  (let ((vt (vtable-of o)))
    (if (and (eq? name 'lookup)
	     (eq? vt vtable-vt))
	(vtable-lookup o name)
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

(define object-vt (let* ((ovt-ph (make-placeholder #f))
			 (vvt-ph (make-placeholder #f))
			 (ovt (simple-vtable (make-hash) #f vvt-ph))
			 (vvt (simple-vtable (make-hash) ovt-ph vvt-ph)))
		    (placeholder-set! ovt-ph ovt)
		    (placeholder-set! vvt-ph vvt)
		    (make-reader-graph ovt)))
(define vtable-vt (simple-vtable-vtable object-vt))

(vtable-add-method! vtable-vt 'lookup vtable-lookup)

(vtable-add-method! vtable-vt 'add-method!
		    (lambda (self name method)
		      (hash-set! (simple-vtable-methods self)
				 name
				 method)))

(send vtable-vt 'add-method! 'delegated
      (lambda (self)
	(simple-vtable (make-hash) self vtable-vt)))
