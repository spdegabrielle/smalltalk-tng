(define (serialize-image!)
  (define seen (make-hash-table eq?))
  (define counter 0)

  (define (lookup o)
    (hash-table-ref seen o))

  (define (store! o)
    (let ((ref counter))
      (set! counter (+ counter 1))
      (hash-table-set! seen o ref)
      ref))

  (define (reference o walker)
    (cond
     ((lookup o))
     (else (let ((ref (store! o)))
	     (vector ref (walker o))))))

  (define (walk-primitive o)
    o)

  (define (walk o)
    (cond
     ((object? o) (reference o walk-object))

     ((or (number? o)
	  (char? o)
	  (symbol? o)
	  (string? o)
	  (boolean? o)
	  (null? o))
      (reference o walk-primitive))

     ((pair? o) (reference o walk-pair))
     ((vector? o) (reference o walk-vector))

     (else
      (if (not (procedure? o))
	  (debug 1 "Pinching off primitive reference: "o))
      (reference '() walk-primitive))))

  (define (walk-pair o)
    (let* ((a (walk (car o)))
	   (d (walk (cdr o))))
      (cons a d)))

  (define (walk-vector o)
    (list->vector (cons 'v (map-in-order walk (vector->list o)))))

  (define (walk-object o)
    (let* ((layout (reference (object-layout o) walk-layout))
	   (slots (map-in-order walk (vector->list (object-slots o))))
	   (category (cond
		      ((and (has-slot? o 'traits)
			    (eq? (get-slot o 'traits) *traits-method*))
		       (cond
			((not (eq? (get-slot o 'primitive) *nil*)) 'p)
			((not (eq? (get-slot o 'accessor) *nil*)) 'a)
			(else 'o)))
		      (else 'o))))
      (vector category layout slots)))

  (define (walk-layout layout)
    (let ((answer '()))
      (layout-for-each layout
		       (lambda (slot-name slot)
			 (push! answer (reference slot walk-slot))))
      (reverse answer)))

  (define (walk-slot slot)
    (list (slot-name slot)
	  (slot-index slot)
	  (slot-delegating? slot)
	  (slot-kind slot)
	  (map-in-order walk-role (slot-roles slot))))

  (define (walk-role role)
    (list (bitset->list (role-positions role))
	  (bitset->list (role-requirements role))
	  (walk (role-method role))))

  (store-globals-to-image!)

  (let ((literals '())
	(roots (map-in-order (lambda (entry)
			       (cons (car entry)
				     (walk (cdr entry))))
			     (hash-table->list *image-root*))))
    (for-each-literal-object (lambda (literal object)
			       (when (or (lookup literal)
					 (assq literal *root-literals*))
				 (let* ((l (walk literal))
					(o (walk object)))
				   (push! literals (cons l o))))))
    (cons roots
	  (reverse literals)))
)

;---------------------------------------------------------------------------

(define (deserialize-image! image)
  (define seen (make-hash-table eq?))
  (define fixups '())

  (define (lookup x)
    (or (hash-table-ref seen x)
	(error "Image format error: out-of-order reference" x)))

  (define (store! n shell fixup)
    (hash-table-set! seen n shell)
    (fixup shell)
    shell)

  (define (dereference x loader)
    (cond
     ((number? x)
      (lookup x))
     ((not (vector? x)) (error "Image format error: bad definition" x))
     (else (loader (vector-ref x 1)
		   (lambda (shell fixup)
		     (store! (vector-ref x 0)
			     shell
			     fixup))))))

  (define (load x)
    (dereference x
		 (lambda (y k)
		   (cond
		    ((vector? y)
		     (case (vector-ref y 0)
		       ((o p a) (k (make-object* #f #f)
				   (make-object-fixup (vector-ref y 0)
						      (vector-ref y 1)
						      (vector-ref y 2))))
		       ((v) (k (make-vector (- (vector-length y) 1))
			       (lambda (shell)
				 (do ((i 0 (+ i 1)))
				     ((= i (vector-length shell)))
				   (vector-set! shell i
						(load (vector-ref y (+ i 1))))))))
		       (else (error "Image format error: illegal compound" y))))
		    ((pair? y) (k (cons #f #f)
				  (lambda (shell)
				    (set-car! shell (load (car y)))
				    (set-cdr! shell (load (cdr y))))))
		    (else (k y (lambda (shell) shell)))))))

  (define (make-object-fixup category layout slots)
    (lambda (shell)
      (set-object-layout! shell (dereference layout load-layout))
      (set-object-slots! shell (list->vector (map-in-order load slots)))
      (case category
	((p) (push! fixups
		    (lambda ()
		      (set-slot! shell 'code (lookup-primitive (get-slot shell 'primitive))))))
	((a) (push! fixups
		    (lambda ()
		      (set-slot! shell 'code
				 (let ((name (get-slot shell 'accessor)))
				   (if (eq? (get-slot shell 'selector) name)
				       (build-getter-body name)
				       (build-setter-body name)))))))
	(else 'pass))))

  (define (load-layout x k)
    (k (make-layout*)
       (lambda (layout)
	 (for-each (lambda (slot)
		     (layout-set! layout (slot-name slot) slot))
		   (map-in-order load-slot x))
	 layout)))

  (define (load-slot x)
    (dereference x
		 (lambda (y k)
		   (let*-structure (((name index delegating? kind roles) y))
		     (k (make-slot* name index #f delegating? kind)
			(lambda (shell)
			  (set-slot-roles! shell (map-in-order load-role roles))))))))

  (define (load-role x)
    (let*-structure (((positions requirements method) x))
      (make-role* (list->bitset positions)
		  (list->bitset requirements)
		  (load method))))

  (set! *image-root* (make-hash-table eq?))
  (flush-literal-objects-table!)

  (let ((roots (car image))
	(literals (cdr image)))
    (for-each (lambda (entry)
		(debug 1 "--- ROOT "(car entry))
		(hash-table-set! *image-root*
				 (car entry)
				 (load (cdr entry))))
	      roots)
    (for-each (lambda (entry)
		(debug 1 "--- LITERAL "entry)
		(let ((literal (load (car entry)))
		      (object (load (cdr entry))))
		  (install-object-for-literal! literal object)))
	      literals))

  (run-hooks! fixups)
  (debug 1 "Done.")

  (load-globals-from-image!)
)
