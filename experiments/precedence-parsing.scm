(define table '((== non 4)
		(: right 5)
		(++ right 5)
		(+ left 6)
		(- left 6)
		(* left 7)
		(/ left 7)))

(define (parse exp)
  (define (p-op lhs exp min-precedence k)
    ;;(write `(p-op ,lhs ,exp ,min-precedence)) (newline)
    (if (null? exp)
	(k lhs exp)
	(let ((op (car exp))
	      (rest0 (cdr exp)))
	  (cond
	   ((assq op table) =>
	    (lambda (entry)
	      (let ((fixity (cadr entry))
		    (prec (caddr entry)))
		(if (>= prec min-precedence)
		    (p-val rest0
			   (lambda (rhs rest)
			     (let loop ((rhs rhs)
					(rest rest))
			       ;;(write `(loop ,rhs ,rest)) (newline)
			       (if (null? rest)
				   (k `(,op ,lhs ,rhs) rest)
				   (let ((lookahead (car rest)))
				     (cond
				      ((assq lookahead table) =>
				       (lambda (lentry)
					 (let ((lfixity (cadr lentry))
					       (lprec (caddr lentry)))
					   (if (or (and (eq? lfixity 'right) (= lprec prec))
						   (> lprec prec))
					       (p-op rhs rest lprec loop)
					       (p-op `(,op ,lhs ,rhs) rest min-precedence k)))))
				      (else (loop `(app ,rhs ,lookahead) (cdr rest))
					    ;; (p-op rhs rest min-precedence
;; 						  (lambda (v r)
;; 						    ;;(write `(loop-n ,v ,r)) (newline)
;; 						    (k `(,op ,lhs ,v) r)))
					    )))))))
		    (k lhs exp)))))
	   (else (p-op `(app ,lhs ,op) rest0 min-precedence k))))))

  (define (p-val exp k)
    (k (car exp) (cdr exp)))

  (p-val exp (lambda (lhs rest)
	       (p-op lhs rest 0 (lambda (result rest)
				  (list 'result! result rest))))))

(write (parse '(1 + 2 + 2.5 : 3 * 4 y z * 6 : a : b : c)))
;;(write (parse '(1 + 2 : 3 * 4 * 6 : foo)))
(newline)

