(require 'srfi-1)

(define (rule-matcher nonterminal)
  (lambda (b) (eq? (car b) nonterminal)))

(define (list->lset = l)
  (apply lset-adjoin = '() l))

(define nonterminal-firsts
  (let ()
    (define (rule-firsts rule)
      (if rule
	  (list->lset eq? (map car (filter pair? (cdr rule))))
	  '()))

    (lambda (nonterminal grammar)
      (let loop ((seen '())
		 (work (list (rule-firsts (assq nonterminal grammar)))))
	(if (null? work)
	    seen
	    (let ((firsts (car work))
		  (remaining-work (cdr work)))
	      (loop (apply lset-adjoin eq? seen firsts)
		    (fold (lambda (rule acc)
			    (if (memq rule seen)
				acc
				(let ((f (rule-firsts (assq rule grammar))))
				  (if (null? f)
				      acc
				      (cons f acc)))))
			  remaining-work
			  firsts))))))))

(define (rule-left-recursive? nonterminal grammar)
  (if (memq nonterminal (nonterminal-firsts nonterminal grammar))
      #t
      #f))

(define (rule-degenerate? nonterminal terminals grammar)
  (let ((firsts (nonterminal-firsts nonterminal grammar)))
    (and (memq nonterminal firsts)
	 (null? (lset-intersection eq? terminals firsts)))))

(define (factor-left-recursion grammar)
  (let* ((nonterminals (list->lset eq? (map car grammar)))
	 (allnames (apply lset-union eq? nonterminals (map cddr grammar)))
	 (terminals (lset-difference eq? allnames nonterminals))

	 (grammar (map (lambda (nonterminal)
			 (cons nonterminal
			       (map cddr (filter (rule-matcher nonterminal) grammar))))
		       nonterminals)))
    (for-each (lambda (nonterminal)
		(display "----------------------------------------")
		(newline)
		(display (list nonterminal '-->first (nonterminal-firsts nonterminal grammar)))
		(newline)
		(display (list nonterminal '-->rec (rule-left-recursive? nonterminal grammar)))
		(newline)
		(display (list nonterminal '-->degen
			       (rule-degenerate? nonterminal terminals grammar)))
		(newline))
	      nonterminals)
    'nothing))

(define g
  (map butlast
       '((toplevel --> expr dot toplevel (0 . 2))
	 (toplevel --> expr dot (0))
	 (toplevel --> expr (0))

	 (expr --> method-definition 0)
	 (expr --> nary 0)
	 (expr --> caret expr (reply 1))

	 (nary --> binary nary-args ,fixup-nary)
	 (nary --> binary)
	 (nary-args --> selector binary nary-args ((0 1) . 2))
	 (nary-args --> selector binary ((0 1)))

	 (binary --> binary binaryop unary (send 1 (0 2)))
	 (binary --> unary 0)
	 (binaryop --> punct 0)

	 (unary --> unary identifier (send 1 (0)))
	 (unary --> value 0)

	 (value --> simple-value 0)
	 (value --> oparen expr cparen 1)
	 (value --> oparen updates cparen (update #f 1))
	 (value --> oparen expr pipe updates cparen (update 1 3))

	 (simple-value --> identifier (ref 0))
	 (simple-value --> identifier oparen updates cparen stateful-block
		       (stateful-block 0 2 . 4))
	 (simple-value --> stateless-block 0)
	 (simple-value --> string (string 0))
	 (simple-value --> symbol (symbol 0))
	 (simple-value --> integer (number 0))

	 (updates --> update updates (0 . 1))
	 (updates --> update (0))
	 (update --> identifier colonequal value (0 2))

	 (stateful-block --> obrack binders stateful-expr-seq cbrack (1 2))
	 (stateful-expr-seq --> stateful-expr dot stateful-expr-seq (0 . 2))
	 (stateful-expr-seq --> stateful-expr (0))
	 (stateful-expr-seq --> ())
	 (stateful-expr --> identifier oparen updates cparen (loop 0 2))
	 (stateful-expr --> expr 0)

	 (stateless-block --> obrack binders expr-seq cbrack (block 1 2))
	 (expr-seq --> let-expr dot expr-seq (0 . 2))
	 (expr-seq --> let-expr (0))
	 (expr-seq --> ())
	 (let-expr --> identifier equal expr (let 0 2))
	 (let-expr --> expr 0)

	 (binders --> binders+ pipe 0)
	 (binders --> ())
	 (binders+ --> binder binders+ (0 . 1))
	 (binders+ --> binder (0))
	 (binder --> colon identifier 1)

	 (method-definition --> method-params obrack expr-seq cbrack (method 0 2))
	 (method-params --> method-param identifier (send 1 (0)))
	 (method-params --> method-param binaryop method-param (send 1 (0 2)))
	 (method-params --> method-param method-nary ,fixup-nary)
	 (method-param --> underscore at value (#f 2))
	 (method-param --> identifier at value (0 2))

	 (method-nary --> selector method-param method-nary ((0 1) . 2))
	 (method-nary --> selector method-param ((0 1)))
	 )))

(define g '((a --> d)
	    (a --> a d)
	    (b --> d)
	    (b --> b d)
	    (d --> e)
	    (e --> f d)
	    (e --> g)))

(define g '((t --> a m)
	    (t --> b n)
	    (a --> c)
	    (b --> c)
	    (c --> d)
	    (c --> a)))

(define g
  (map butlast
       '((sum --> sum + val ,(lambda (a b c) (+ a c)))
	 (sum --> val val ,(lambda (a b) a))
	 (val --> num ,(lambda (a) a)))))

(define g
  (map butlast
       '((sum --> val val sumk ,(lambda (a b k) (k a)))
	 (val --> num ,(lambda (a) a))
	 (sumk --> + val ,(lambda (b c) (lambda (a) (+ a c))))
	 (sumk --> ,(lambda () (lambda (a) a))))))

(pretty-print (factor-left-recursion g))
(exit)
