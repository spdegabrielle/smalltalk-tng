;; 13 April 2006, 0700 - 0840

(require (lib "match.ss")
	 (lib "pretty.ss")
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
  (syntax-rules (+ / @ - = ^)
    ((_ (+ t ...)) (sum-trait (list (trait-expr t) ...)))
    ((_ (/ t ...)) (override-trait (list (trait-expr t) ...)))
    ((_ (@ t (new old) ...)) (rename-trait (trait-expr t)
					   '((new old) ...)
					   (lambda (self-and-args)
					     (match self-and-args
					       (new (list . old)) ...
					       (_ self-and-args)))))
    ((_ (- t pattern ...)) (subtract-trait (trait-expr t)
					   '(pattern ...)
					   (lambda (self-and-args)
					     (match self-and-args
					       (pattern #t) ...
					       (_ #f)))))
    ((_ (= expr)) expr)
    ((_ (^ pattern body ...)) (trait* (((self . pattern) body ...))))
    ((_ ((pattern body ...) ...)) (simple-trait '(pattern ...)
						(lambda (self-and-args)
						  (match self-and-args
						    (pattern (lambda () (trait-expr body) ...))
						    ...
						    (_ #f)))))
    ((_ var) var)))

(define-syntax trait-expr
  (syntax-rules (quote let if)
    ((_ #(item ...))
     (trait* (item ...)))

    ((_ (quote v))
     (quote v))

    ((_ (let ((pattern expr) ...) body ...))
     (trait-expr (let dummy ((pattern expr) ...) body ...)))

    ((_ (let loop ((pattern expr) ...) body ...))
     (trait-expr (#(((loop pattern ...) body ...))
		  expr ...)))

    ((_ (receiver arg ...))
     (traits-send (trait-expr receiver) (list (trait-expr arg) ...)))

    ((_ other)
     other)))

(define-syntax define-trait
  (syntax-rules ()
    ((_ name traitbody) (define name (trait-expr traitbody)))))

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

(define (traits-for o)
  (cond
   ((trait? o) o)
   ((number? o) <number>)
   ((string? o) <string>)
   ((symbol? o) <symbol>)
   ((boolean? o) <boolean>)
   ((pair? o) (trait-expr (<pair> 'cons (car o) (cdr o))))
   ((null? o) <nil>)
   ((procedure? o) (simple-trait '(any) (lambda (self-and-args) (lambda () (apply o (cdr self-and-args))))))))

(define (invoke-method traits self args)
  (let ((method (search-trait traits (cons self args))))
    (cond
     ((procedure? method) (method))
     ((not method) (error "Does not understand" args self))
     ((eq? method 'conflict) (error "Method conflict" args self))
     (else (error "Unknown search-trait result" method args self)))))

(define (lookup-method self args)
  (let ((method (search-trait self (cons self args))))
    (and (procedure? method)
	 method)))

(define (traits-send o args)
  (invoke-method (traits-for o) o args))

(define-trait <extendable>
  #(((self 'extend-with trait) #(/ trait (self 'species)))))

(define-trait <sequenceable>
  #(((self 'map f) (self 'cons (f (self 'first)) ((self 'rest) 'map f)))
    ((self 'foldl f 'with seed) ((self 'rest) 'foldl f 'with (f (self 'first) seed)))
    ((self 'foldr f 'with seed) (f (self 'first) ((self 'rest) 'foldr f 'with seed)))))

(define-trait <consable>
  #(((self 'cons f r) (self 'extend-with #(((self 'first) f)
					   ((self 'rest) r)
					   ((self 'empty?) #f))))))

(define-trait <empty-sequenceable>
  #(((self 'map f) self)
    ((self 'foldl f 'with seed) seed)
    ((self 'foldr f 'with seed) seed)
    ((self 'empty?) #t)))

(define-trait <boolean>
  #(((self 'ifTrue t) #(= (if self (trait-expr (t)) #f)))
    ((self 'ifFalse f) #(= (if self #f (trait-expr (f)))))
    ((self 'ifTrue t 'ifFalse f) #(= (if self (trait-expr (t)) (trait-expr (f)))))))

(define tmap (trait-expr #(((self f seq)
			    ((seq 'empty?)
			     'ifTrue #(^() seq)
			     'ifFalse #(^() (seq 'cons (f (seq 'first)) (self f (seq 'rest)))))))))

(define-syntax define-prototype
  (syntax-rules ()
    ((_ n traitbody) (define n (trait-expr #(/ #(((self 'species) n)
						 ((self 'name) 'n))
					       traitbody))))))

(define-prototype <nil> <empty-sequenceable>)
(define-prototype <pair> #(+ <sequenceable> <consable> <extendable>))

(define (kons f r)
  (trait-expr (<pair> 'cons f r)))

(define (liszt . l)
  (fold-right kons <nil> l))

(define (expand-series exp)
  (pretty-print exp)(newline)
  (let loop ((stx (expand-once exp)))
    (pretty-print (syntax-object->datum stx))(newline)
    (let ((next (expand-once stx)))
      (when (not (equal? (syntax-object->datum stx)
			 (syntax-object->datum next)))
	(loop next)))))
