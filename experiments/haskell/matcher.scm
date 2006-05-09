;; Experimental reimplementation of matcher.hs in scheme
;; Not properly lazy at the moment

(require (lib "1.ss" "srfi")
	 (lib "etc.ss")
	 (lib "trace.ss")
	 (lib "match.ss"))

(define (parse x)
  (cond
   ((eq? x '_) `(discard))
   ((symbol? x) (let ((name (symbol->string x)))
		  (if (char=? (string-ref name 0) #\+)
		      `(binding ,(string->symbol (substring name 1 (string-length name)))
				(discard))
		      `(atom ,x))))
   ((number? x) `(atom ,x))
   (else (match x
	   ((b '@ p) (match (parse b)
		       (('binding name _) `(binding ,name ,(parse p)))
		       (else (error "Syntax error in @" x))))
	   ((i '= p) `(let ,i ,(parse p)))
	   ((a ': b more ...) (let loop ((y x) (acc '()))
				(match y
				  (() `(object ,(reverse acc)))
				  ((a ': b more ...) (loop more (cons (cons (parse a) (parse b))
								      acc)))
				  (else (error "Syntax error in object" x)))))
	   ((op part ...) (fold (lambda (part op) `(app ,op ,(parse part)))
				(parse op)
				part))
	   (else "Syntax error" x)))))

(define (show x)
  (match x
    (('atom s) (cond
		((symbol? s) (symbol->string s))
		((number? s) (number->string s))
		(else "#<ATOM>")))
    (('binding name ('discard)) (list "+" (symbol->string name)))
    (('binding name p) (list "+" (symbol->string name) (show p)))
    (('discard) "_")
    (('object ()) "[]")
    (('object (clause)) (list "[" (show-clause clause) "]"))
    (('object (clause clauses ...)) (list "[" (show-clause clause)
					  (map (lambda (c) (list " " (show-clause c))) clauses)
					  "]"))
    (else "#<UNKNOWN>")))

(define (show-clause c)
  (list (show (car c)) ": " (if (cadr c) "{code}" (show (tng-reduce (cdr c) '())))))

(define (tng-write x . maybe-port)
  (let walk ((x (show x)))
    (cond
     ((null? x) 'ignore)
     ((pair? x) (begin (walk (car x)) (walk (cdr x))))
     (else (apply display x maybe-port)))))

(define (tng-match a b fk sk)
  (match a
    (('atom _) (if (equal? a b) (sk '()) (fk)))
    (('binding name pat) (tng-match pat b fk (lambda (bs) (sk (cons (cons name (delay b)) bs)))))
    (('discard) (sk '()))
    (('object pattern-clauses)
     (if (eq? (first b) 'object)
	 (let ((value-clauses (second b)))
	   (let loop ((pattern-clauses pattern-clauses)
		      (bs '()))
	     (if (null? pattern-clauses)
		 (sk bs)
		 (tng-match1 value-clauses
			     (car pattern-clauses)
			     fk
			     (lambda (bs1) (loop (cdr pattern-clauses) (append bs1 bs)))))))
	 (fk)))
    (else (fk))))

(define (tng-match1 value-clauses pattern-clause fk sk)
  (if (null? value-clauses)
      (fk)
      (let ((fk1 (lambda () (tng-match1 (cdr value-clauses) pattern-clause fk sk))))
	(match (car value-clauses)
	  ((vpat . vval) (match pattern-clause
			   ((pval . ppat)
			    (tng-match vpat pval fk1
				       (lambda (bs1)
					 (tng-match (tng-reduce ppat '())
						    (tng-reduce vval bs1)
						    fk1
						    sk))))))))))

(define (tng-reduce closure bindings)
  ((cdr closure) bindings))

(define (tng-eval bindings o)
  (match o
    (('atom s) (cond ((assq s bindings) => (compose force cdr))
		     ((assq s *tng-globals*) => (compose force cdr))
		     (else o)))
    (('binding name ast) `(binding ,name ,(tng-eval bindings ast)))
    (('discard) `(discard))
    (('object clauses) `(object ,(map (match-lambda ((patexp . val)
						     (let ((pat (tng-eval bindings patexp)))
						       (cons pat (maybe-close pat bindings val)))))
				      clauses)))
    (('let name ast) (letrec ((result (tng-eval (cons (cons name (delay result)) bindings) ast)))
		       result))
    (('app rator rand) (tng-apply bindings (tng-eval bindings rator) (tng-eval bindings rand)))))

(define (maybe-close pat bindings val)
  (if (has-bindings? pat)
      (cons #t (lambda (new-bindings) (tng-eval (append new-bindings bindings) val)))
      (let ((v (delay (tng-eval bindings val))))
	(cons #f (lambda (new-bindings) (force v))))))

(define (has-bindings? p)
  (match p
    (('atom _) #f)
    (('binding _ p1) #t)
    (('discard) #f)
    (('object clauses) (any (lambda (clause) (has-bindings? (tng-reduce (cdr clause) '())))
			    clauses))))

(define (dnu fn val)
  (error "Does Not Understand" `((fn ,fn) (val ,val))))

(define (tng-apply bindings function value)
  (match function
    (('object pattern-clauses)
     (let loop ((pattern-clauses pattern-clauses))
       (if (null? pattern-clauses)
	   (dnu function value)
	   (match (car pattern-clauses)
	     ((ppat . pval) (tng-match ppat value (lambda () (loop (cdr pattern-clauses)))
				       (lambda (bs) (tng-reduce pval bs))))))))
    (else (dnu function value))))

(define *tng-globals*
  (map (match-lambda ((name exp)
		      `(,name . ,(delay (tng-eval '() (parse exp))))))
       `(
	 (cons (+a : (+d : (first : a rest : d))))
	 (map (+f : (loop = ((cons +a +d) : (cons (f a) (loop d))
			     +x : x))))
	 )))

;(trace parse tng-eval tng-match tng-match1 tng-reduce tng-apply)
;(trace tng-apply)
