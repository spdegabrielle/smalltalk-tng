(define *debug-level* 0)
(define *debug-indent* 0)

(define (external-representation o)
  (let ((p (open-output-string)))
    (write o p)
    (get-output-string p)))

(define (fold-left/index fn acc lis)
  (let loop ((index 0)
	     (lis lis)
	     (acc acc))
    (if (null? lis)
	acc
	(loop (+ index 1)
	      (cdr lis)
	      (fn index (car lis) acc)))))

(define (for-each/index fn lis)
  (let loop ((index 0)
	     (lis lis))
    (unless (null? lis)
      (fn index (car lis))
      (loop (+ index 1) (cdr lis)))))

(define (describe-object o . pretty)
  (let ((description (map (lambda (entry)
			    (let ((key (car entry))
				  (val (cdr entry)))
			      (list (slot-name val)
				    (slot-index val)
				    (slot-delegating? val)
				    (slot-kind val)
				    (map (lambda (role)
					   (list (role-positions role)
						 (role-requirements role)
						 (role-method role)))
					 (slot-roles val)))))
			  (hash-table->list (object-layout o)))))
    (if (or (null? pretty) (car pretty))
	(pretty-print description))
    description))

(define (send/previous-method/missing-handler previous-method missing-handler selector argv)
  (let* ((method (dispatch previous-method selector argv)))
    (debug 2 --> 0 "Dispatching to method "method)
    (if method
	(let ((code (get-slot method 'code)))
	  (if (procedure? code)
	      (apply code method (vector->list argv))
	      (metalevel-eval-method code method argv)))
	(missing-handler argv))))

(define (send/previous-method previous-method selector argv)
  (send/previous-method/missing-handler previous-method
					(lambda (argv)
					  (send/previous-method/missing-handler
					   #f
					   (lambda (inner-argv)
					     (error "Dispatch failed"
						    `(send ,selector ,@(vector->list argv))))
					   'notFoundOn:
					   (vector selector argv)))
					selector
					argv))

(define (run-hooks! hooklist)
  (for-each (lambda (hook) (hook)) (reverse hooklist)))

(define (curry f . vs)
  (lambda rest
    (apply f (append vs rest))))

(define (non-*false*? x)
  (if (eq? x *false*)
      #f
      x))

(define (*false*? x)
  (eq? x *false*))

(define (vector-fold fn seed v)
  (let ((len (vector-length v)))
    (do ((i 0 (+ i 1))
	 (seed seed (fn (vector-ref v i) seed)))
	((= i len) seed))))
