(define show-parens
  (lambda (x mode)
    (case mode
      ((eval) (list "(" x ")"))
      ((quote) (list "[" x "]"))
      ((meta-quote) (list "{" x "}"))
      (else (error "Unknown show-mode" mode)))))

(define show-join
  (lambda (mid xs)
    (if (null? xs)
	'()
	(reverse (fold (lambda (x acc)
			 (cons x (cons mid acc)))
		       (list (car xs))
		       (cdr xs))))))

(define show-tng
  (lambda (c mode)
    (let walk ((c c))
      (case (car c)
	((tuple) (show-join ", " (map walk (cdr c))))
	((atom lit) (let ((o (open-output-string)))
		      (display (cadr c) o)
		      (get-output-string o)))
	((adj) (list (walk (cadr c)) " " (walk (caddr c))))
	((fun) (show-join " " (map (lambda (entry)
				     (list (show-tng (car entry) 'quote) ": "
					   (show-tng (cadr entry) 'eval)))
				   (cdr c))))
	((eval) (show-parens (show-tng (cadr c) 'eval) 'eval))
	((quote) (if (eq? (car (cadr c)) 'atom)
		     (list "#" (symbol->string (cadr (cadr c))))
		     (show-parens (show-tng (cadr c) 'quote) 'quote)))
	((meta-quote) (show-parens (show-tng (cadr c) 'meta-quote) 'meta-quote))
	((discard) "_")
	(else (error "Unknown term in show-tng" c))))))

(define print-tng
  (lambda (c mode)
    (let walk ((x (show-tng c mode)))
      (cond
       ((null? x))
       ((pair? x) (walk (car x)) (walk (cdr x)))
       (else (display x))))))
