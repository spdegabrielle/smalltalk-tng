;; 13 April 2006, 0700 - 0840

(require (lib "match.ss")
	 (lib "1.ss" "srfi")
	 (lib "9.ss" "srfi"))

(print-struct #t)
(define previous-inspector (current-inspector))
(current-inspector (make-inspector))

(define-record-type <trait>
  (make-trait operator operands)
  trait?
  (operator trait-operator)
  (operands trait-operands))

(current-inspector previous-inspector)

(define (simple-trait patterns search-proc)
  (make-trait 'simple (list patterns search-proc)))

(define (sum-trait traits)
  (make-trait 'sum traits))

(define (override-trait traits-in-derived-to-base-order)
  (make-trait 'override traits-in-derived-to-base-order))

(define (rename-trait t mapping mapping-proc)
  (make-trait 'rename (list t mapping mapping-proc)))

(define (subtract-trait t patterns search-proc)
  (make-trait 'subtract (list t patterns search-proc)))

(define-syntax trait*
  (syntax-rules (+ / @ - =)
    ((_ (+ t ...)) (sum-trait (list (trait* t) ...)))
    ((_ (/ t ...)) (override-trait (list (trait* t) ...)))
    ((_ (@ t (new old) ...)) (rename-trait (trait* t)
					   '((new old) ...)
					   (lambda (self-and-args)
					     (match self-and-args
					       (new (list . old)) ...
					       (_ self-and-args)))))
    ((_ (- t pattern ...)) (subtract-trait (trait* t)
					   '(pattern ...)
					   (lambda (self-and-args)
					     (match self-and-args
					       (pattern #t) ...
					       (_ #f)))))
    ((_ (= expr)) expr)
    ((_ ((pattern body ...) ...)) (simple-trait '(pattern ...)
						(lambda (self-and-args)
						  (match self-and-args
						    (pattern (lambda () body ...))
						    ...
						    (_ #f)))))
    ((_ var) var)))

(define-syntax define-trait
  (syntax-rules ()
    ((_ name traitbody) (define name (trait* traitbody)))))

(define (search-trait trait self-and-args)
  (let walk ((trait trait))
    (let ((operands (trait-operands trait)))
      (case (trait-operator trait)
	((simple) ((cadr operands) self-and-args))
	((sum) (let ((candidates (filter-map walk operands)))
		 (cond
		  ((null? candidates) #f)
		  ((pair? (cdr candidates)) 'conflict)
		  (else (car candidates)))))
	((override) (let loop ((operands operands))
		      (cond
		       ((null? operands) #f)
		       ((walk (car operands)))
		       (else (loop (cdr operands))))))
	((rename) (let ((new-self-and-args ((caddr operands) self-and-args)))
		    (search-trait (car operands) new-self-and-args)))
	((subtract) (if ((caddr operands) self-and-args)
			#f
			(walk (car operands))))
	(else (error "Bad trait operator" (trait-operator trait)))))))

(define (make-instance trait)
  (letrec ((self (lambda args
		   (if (null? args)
		       trait
		       (let* ((self-and-args (cons self args))
			      (method (search-trait trait self-and-args)))
			 (cond
			  ((procedure? method) (method))
			  ((not method) (error "Does not understand" args self trait))
			  ((eq? method 'conflict) (error "Method conflict" args self trait))
			  (else (error "Unknown search-trait result" method args self trait))))))))
    self))

(define (instance-trait instance)
  (instance))

(define (lookup-method instance . args)
  (let* ((trait (instance-trait instance))
	 (method (search-trait trait (cons instance args))))
    (and (procedure? method)
	 method)))

(define (extend base trait)
  (make-instance (trait* (/ (= trait)
			    (= (instance-trait base))))))

(define-trait <extendable>
  (((self 'extend t) (extend (self 'species) t))))

(define-trait <sequenceable>
  (((self 'map f) (self 'cons (f (self 'first)) ((self 'rest) 'map f)))
   ((self 'foldl f 'with seed) ((self 'rest) 'foldl f 'with (f (self 'first) seed)))
   ((self 'foldr f 'with seed) (f (self 'first) ((self 'rest) 'foldr f 'with seed)))))

(define-trait <consable>
  (((self 'cons f r) (self 'extend (trait* (((self 'first) f)
					    ((self 'rest) r)))))))

(define-trait <empty-sequenceable>
  (((self 'map f) self)
   ((self 'foldl f 'with seed) seed)
   ((self 'foldr f 'with seed) seed)))

(define-syntax define-prototype
  (syntax-rules ()
    ((_ n traitbody) (define n (make-instance (trait* (/ (((self 'species) n)
							  ((self 'name) 'n))
							 traitbody)))))))

(define-prototype <nil> <empty-sequenceable>)
(define-prototype <pair> (+ <sequenceable> <consable> <extendable>))

(define (kons f r)
  (<pair> 'cons f r))

(define (liszt . l)
  (fold-right kons <nil> l))
