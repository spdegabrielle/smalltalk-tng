(keyword-style '#:none)

(define-syntax compile-if
  (syntax-rules ()
    ((_ #f tb fb) fb)
    ((_ #t tb fb) tb)
    ((_ #f tb) 'conditionally-compiled-away)
    ((_ #t tb) tb)))

(compile-if #t
	    (begin
	      (define-syntax begin/debug-indent
		(syntax-rules ()
		  ((_ body ...)
		   (fluid-let ((*debug-indent* (+ *debug-indent* 2)))
		     (begin body ...)))))

	      (define-syntax debug
		(syntax-rules (-->)
		  ((_ level --> l2 exp ...)
		   (when (>= *debug-level* level)
		     (let ((old-level *debug-level*))
		       (set! *debug-level* l2)
		       (if (positive? *debug-indent*)
			   (display (make-string *debug-indent* #\space)))
		       (display exp) ...
		       (newline)
		       (set! *debug-level* old-level))))
		  ((_ level exp ...)
		   (when (>= *debug-level* level)
		     (if (positive? *debug-indent*)
			 (display (make-string *debug-indent* #\space)))
		     (display exp) ...
		     (newline))))))

	    (begin
	      (define-syntax begin/debug-indent
		(syntax-rules ()
		  ((_ body ...)
		   (begin body ...))))

	      (define-syntax debug
		(syntax-rules (-->)
		  ((_ level --> l2 exp ...)
		   'conditionally-compiled-away)
		  ((_ level exp ...)
		   'conditionally-compiled-away)))))

(define-syntax send
  (syntax-rules ()
    ((_ selector arg ...)
     (send/previous-method #f 'selector (vector arg ...)))))

(define-syntax push!
  (syntax-rules ()
    ((_ variable value)
     (set! variable (cons value variable)))))

(define-syntax let*-structure
  (syntax-rules ()
    ;; minor optimisation - removes a layer of (let)
    ((_ () body)
     body)

    ((_ () body ...)
     (let () body ...))

    ((_ ((pattern value) more ...) body ...)
     (let ((temp value))
       (let*-structure "ONE" pattern temp (more ...) (begin body ...))))

    ((_ "ONE" () value more continuation)
     (if (null? value)
	 (let*-structure more continuation)
	 (error "Pattern mismatch" () value)))

    ((_ "ONE" (left . right) value more continuation)
     (let ((l (car value))
	   (r (cdr value)))
       (let*-structure "ONE" left l () (let*-structure "ONE" right r more continuation))))

    ((_ "ONE" var value more continuation)
     (let ((var value))
       (let*-structure more continuation)))))
