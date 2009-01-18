(define-macro prim0
  (lambda (x env exp)
    (let ((name (cadr x)))
      `(define-global! ',name
	 (lambda ()
	   (%assemble () ()
	     (scheme (,name))))))))

(define-macro prim1
  (lambda (x env exp)
    (let ((name (cadr x)))
      `(define-global! ',name
	 (lambda (a0)
	   (%assemble (a0) (a0)
	     (scheme (,name a0))))))))

(define-macro prim2
  (lambda (x env exp)
    (let ((name (cadr x)))
      `(define-global! ',name
	 (lambda (a0 a1)
	   (%assemble (a0 a1) (a0 a1)
	     (scheme (,name a0 a1))))))))

(prim1 primitive-eval)
(prim1 read-file)
(prim1 write)
(prim1 newline)

(prim1 cdr)
(prim1 cddr)
(prim1 cddar)
(prim1 cdar)
(prim1 cdadr)
(prim1 car)
(prim1 cadr)
(prim1 caddr)
(prim1 cadddr)
(prim1 cadar)
(prim1 caar)
(prim1 caadr)

(prim1 box)
(prim1 unbox)
(prim2 set-box!)
    
(prim1 length)
(prim2 append)
(prim1 reverse)
(prim2 cons)
(prim2 eq?)
(prim2 =)
(prim1 not)
(prim1 null?)
(prim1 pair?)
(prim1 symbol?)

(prim0 gensym)

(define-global! 'map
  (lambda (f l)
    (if (null? l)
	'()
	(cons (f (car l))
	      (map f (cdr l))))))

(define-global! 'for-each
  (lambda (f l)
    (if (null? l)
	'ok
	(begin (f (car l))
	       (for-each f (cdr l))))))
