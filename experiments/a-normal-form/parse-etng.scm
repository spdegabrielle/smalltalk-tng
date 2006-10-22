(define parse-etng
  (let* ((nonquote (lambda (ch) (not (eqv? ch #\"))))

	 (parser
	  (packrat-parse
	   `(

	     (toplevel (ws semis (m <- sequence-member semis)+ #f ,(packrat-lambda (m) m)))
	     (toplevel1 (ws semis m <- sequence-member #\; ,(packrat-lambda (m) m)))

	     (expr (/ sequence
		      send))

	     (send ((e <- simple-expr)+ ,(packrat-lambda (e)
					   (fold (lambda (operand operator)
						   (make-node 'core-send
							      'receiver operator
							      'message operand))
						 (car e)
						 (cdr e)))))

	     (simple-expr (/ object
			     (OPAREN o <- operator CPAREN ,(packrat-lambda (o)
							     (make-node 'core-ref 'name o)))
			     tuple
			     (l <- literal ,(packrat-lambda (l) (make-node 'core-lit 'value l)))
			     ("self"ws ,(packrat-lambda () (make-node 'core-self)))
			     ("parent"ws m <- simple-expr
			      ,(packrat-lambda (m) (make-node 'core-send-to-parent 'message m)))
			     ("lazy"ws bs <- lazy-bindings "in"ws e <- expr
			      ,(packrat-lambda (bs e) (make-node 'core-lazy 'bindings bs 'body e)))
			     (i <- id ,(packrat-lambda (i) (make-node 'core-ref 'name i)))
			     (OPAREN e <- expr CPAREN ,(packrat-lambda (e) e))))

	     (lazy-bindings (OPAREN ws semis (bs <- lazy-binding semis)* CPAREN
				    ,(packrat-lambda (bs) bs)))

	     (lazy-binding (i <- id EQ e <- expr
			      ,(packrat-lambda (i e) (make-node 'core-lazy-binding
								'name i 'value e))))

	     (literal (/ (#\. ws i <- id ,(packrat-lambda (i) i))
			 (o <- operator ,(packrat-lambda (o) o))
			 (w <- word ,(packrat-lambda (w) w))))

	     (object (#\[ ws (m <- member)* #\] ws
		      ,(packrat-lambda (m) (make-node 'core-object 'methods m))))

	     (member (/ constant-member
			method-member))

	     (constant-member (p <- pattern EQ e <- expr semis
			       ,(packrat-lambda (p e) (make-node 'core-constant
								 'pattern p 'body e))))
	     (method-member (p <- pattern ARROW e <- expr semis
			       ,(packrat-lambda (p e) (make-node 'core-method
								 'pattern p 'body e))))

	     (sequence (#\{ ws semis (e <- sequence-member semis)* #\} ws
			,(packrat-lambda (e) (make-node 'core-sequence 'exps e))))

	     (sequence-member (/ (p <- pattern EQ e <- expr
				    ,(packrat-lambda (p e)
				       (make-node 'core-let 'pattern p 'value e)))
				 ;;assignment
				 expr))

; 	     (assignment (s <- send COLONEQ e <- expr
; 			    ,(packrat-lambda (s e) (make-node 'assignment s e))))

	     (tuple (/ (OPAREN CPAREN ,(packrat-lambda () (make-node 'core-tuple 'elements '())))
		       (OPAREN e <- expr (#\, ws es <- expr)+ CPAREN
			       ,(packrat-lambda (e es)
				  (make-node 'core-tuple 'elements (cons e es))))))

	     (pattern (/ (OPAREN o <- operator CPAREN ,(packrat-lambda (o)
							 (make-node 'pat-binding 'name o)))
			 pat-tuple
			 (#\_ ws ,(packrat-lambda () (make-node 'pat-discard)))
			 (l <- literal ,(packrat-lambda (l) (make-node 'pat-lit 'value l)))
			 (i <- id ,(packrat-lambda (i) (make-node 'pat-binding 'name i)))
			 (OPAREN p <- pattern CPAREN ,(packrat-lambda (p) p))))

	     (pat-tuple (/ (OPAREN CPAREN ,(packrat-lambda () (make-node 'pat-tuple 'elements '())))
			   (OPAREN p <- pattern (#\, ws ps <- pattern)+ CPAREN
				   ,(packrat-lambda (p ps)
				      (make-node 'core-tuple 'elements (cons p ps))))))

	     ;;---------------------------------------------------------------------------

	     (semis (SEMI *))

	     (id ((! #\_) (a <- id-alpha) (r <- (/ id-alpha digit))* ws
		  ,(packrat-lambda (a r) (string->symbol (list->string (cons a r))))))

	     (operator ((! reserved-operator)
			a <- op-punct (r <- (/ op-punct digit alpha))* ws
			,(packrat-lambda (a r) (string->symbol (list->string (cons a r))))))

	     (word (/ positive-word
		      (#\- ws w <- positive-word ,(packrat-lambda (w) (- w)))))
	     (positive-word ((d <- digit)+ ws
			     ,(packrat-lambda (d) (string->number (list->string d)))))

	     (id-alpha (/ alpha #\_ #\$))
	     (op-punct (/: ":!#%&*+/<=>?@\\^|-~"))

	     (ws (/ ((/: ,char-whitespace? "whitespace")+ ws)
		    (#\" (/: ,nonquote "comment character")* #\" ws)
		    ()))
	     (digit (/: ,char-numeric? "digit"))
	     (alpha (/: ,char-alphabetic? "letter"))

	     (reserved-operator (/ ARROW
				   COLONEQ
				   EQ))

	     (ARROW ("->" (! op-punct) ws))
	     (COLONEQ (":=" (! op-punct) ws))
	     (EQ (#\= (! op-punct) ws))

	     (SEMI (#\; ws))
	     (OPAREN (#\( ws))
	     (CPAREN (#\) ws))

	     ))))
    (lambda (results k-ok k-fail)
      (try-packrat-parse-pattern
       (parser 'toplevel1) '() results
       (lambda (bindings result) (k-ok (parse-result-semantic-value result)
				       (parse-result-next result)))
       (lambda (err) (k-fail (list (parse-position->string (parse-error-position err))
				   (parse-error-expected err)
				   (parse-error-messages err))))))))

;;; Local Variables:
;;; eval: (put 'packrat-lambda 'scheme-indent-function 1)
;;; End:
