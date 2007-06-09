(define parse-etng
  (let* ((nonquote (lambda (ch) (not (eqv? ch #\"))))
	 (non-string-quote (lambda (ch) (not (eqv? ch #\'))))

	 (stream-ns-uri "http://eighty-twenty.org/etng/r1/ns/stream")
	 (string-ns-uri "http://eighty-twenty.org/etng/r1/ns/string")

	 (stream-cons-name (make-qname stream-ns-uri 'cons))
	 (stream-cons-ref (make-node 'core-ref 'name stream-cons-name))
	 (stream-nil-name (make-qname stream-ns-uri 'nil))
	 (stream-nil-ref (make-node 'core-ref 'name stream-nil-name))
	 (expand-stream
	  (lambda (prefix suffix)
	    (fold-right (lambda (p acc)
			  (make-node 'core-send
				     'receiver stream-cons-ref
				     'message (make-node 'core-tuple
							 'elements (list p acc))))
			(or suffix stream-nil-ref)
			prefix)))

	 (stream-next-name (make-qname stream-ns-uri 'next))
	 (stream-next-lit (make-node 'pat-lit 'value stream-next-name))
	 (stream-empty-name (make-qname stream-ns-uri 'empty))
	 (stream-empty-message
	  (make-node 'pat-message
		     'parts (list (make-node 'pat-lit 'value stream-empty-name))))
	 (expand-stream-pattern
	  (lambda (prefix suffix)
	    (fold-right (lambda (p acc)
			  (make-node 'pat-message
				     'parts (list stream-next-lit
						  (make-node 'pat-tuple
							     'elements (list p acc)))))
			(or suffix stream-empty-message)
			prefix)))

	 (string->u8vector/utf-8
	  (lambda (str)
	    (list->u8vector (bytes->list (string->bytes/utf-8 str)))))

	 (string-stream-name (make-qname string-ns-uri 'stream))
	 (string-stream-ref (make-node 'core-ref 'name string-stream-name))
	 (stream-over-string
	  (lambda (str)
	    (let ((bytes (string->u8vector/utf-8 str)))
	      (make-node 'core-send
			 'receiver string-stream-ref
			 'message (make-node 'core-lit 'value bytes)))))

	 (parser
	  (packrat-parse
	   `(

	     (toplevel (ws semis (m <- command semis)+ #f ,(packrat-lambda (m) m)))
	     (toplevel1 (ws semis m <- command #\; ,(packrat-lambda (m) m)))

	     (command (/ (def <- namespace-declaration
			      ,(packrat-lambda (def)
				 (make-node 'command-define-namespace
					    'prefix (car def)
					    'uri (cdr def))))
			 (DEFINE p <- tuple-pattern EQ e <- expr
			  ,(packrat-lambda (p e)
			     (make-node 'command-define-values
					'pattern p
					'value e)))
			 (DEFINE n <- qname (args <- pattern)* EQ e <- expr
			  ,(packrat-lambda (n args e)
			     (make-node 'command-define-object
					'name n
					'args args
					'body e)))
			 (e <- expr
			    ,(packrat-lambda (e)
			       (make-node 'command-exp
					  'value e)))))

	     (namespace-declaration (NAMESPACE prefix <- id EQ uri <- string
				     ,(packrat-lambda (prefix uri)
					(cons prefix uri))))

	     (expr (/ sequence
		      tuple-value))

	     (tuple-value (es <- comma-separated-exprs
			      ,(packrat-lambda (es)
				 (if (and (pair? es)
					  (null? (cdr es)))
				     (car es)
				     (make-node 'core-tuple 'elements es)))))

	     (comma-separated-exprs (/ (e <- send (#\, ws es <- send)*
					  ,(packrat-lambda (e es) (cons e es)))
				       ,(packrat-lambda () '())))

	     (send ((e <- simple-expr)+ ,(packrat-lambda (e)
					   (fold (lambda (operand operator)
						   (make-node 'core-send
							      'receiver operator
							      'message operand))
						 (car e)
						 (cdr e)))))

	     (simple-expr (/ object
			     function
			     message
			     stream
			     meta
			     (SELF ,(packrat-lambda () (make-node 'core-self)))
			     (NEXTMETHOD ,(packrat-lambda () (make-node 'core-next-method)))
			     (q <- qname ,(packrat-lambda (q) (make-node 'core-ref 'name q)))
			     (l <- literal ,(packrat-lambda (l) (make-node 'core-lit 'value l)))
			     ;; Bug: no corresponding string pattern-match syntax.
			     (s <- string ,(packrat-lambda (s) (stream-over-string s)))
			     (OPAREN e <- expr CPAREN ,(packrat-lambda (e) e))))

	     (literal (/ (#\. ws q <- qname-nooperator ,(packrat-lambda (q) q))
			 (o <- operator ,(packrat-lambda (o) o))
			 (w <- word ,(packrat-lambda (w) w))))

	     (object (OBRACK (m <- member)* CBRACK
			     ,(packrat-lambda (m) (make-node 'core-object 'methods m))))

	     (function (/ (OBRACE (m <- member)* CBRACE
			   ,(packrat-lambda (m) (make-node 'core-function 'methods m)))
			  (OBRACE e <- expr semis CBRACE
				  ,(packrat-lambda (e)
				     (let ((pat (make-node 'pat-tuple 'elements '())))
				       (make-node 'core-function
						  'methods (list (make-node 'core-method
									    'patterns (list pat)
									    'body e))))))))

	     (message (OANGLE (es <- message-component)* CANGLE
		       ,(packrat-lambda (es) (make-node 'core-message 'parts es))))

	     (message-component ((! #\>) simple-expr))

	     (stream (OBRACK p <- comma-separated-exprs s <- stream-suffix
			     ,(packrat-lambda (p s) (expand-stream p s))))

	     (stream-suffix (/ (CBRACK ,(packrat-lambda () #f))
			       (PIPE CBRACK ,(packrat-lambda () #f))
			       (PIPE e <- send CBRACK ,(packrat-lambda (e) e))))

	     (meta (META s <- sexp ,(packrat-lambda (s) (make-node 'core-meta 'sexp s))))

	     (member (/ constant-member
			method-member))

	     (constant-member (ps <- patterns EQ e <- expr semis
				  ,(packrat-lambda (ps e) (make-node 'core-constant
								     'patterns ps 'body e))))
	     (method-member (ps <- patterns ARROW e <- expr semis
				,(packrat-lambda (ps e) (make-node 'core-method
								   'patterns ps 'body e))))

	     (sequence (/ (def <- namespace-declaration semis e <- expr
			       ,(packrat-lambda (def)
				  (make-node 'core-namespace
					     'prefix (car def)
					     'uri (cdr def)
					     'value e)))
			  (LET p <- tuple-pattern EQ e <- expr semis b <- expr
			   ,(packrat-lambda (p e b)
			      (make-node 'core-let
					 'pattern p
					 'value e
					 'body b)))
			  (DO head <- expr semis tail <- expr
			   ,(packrat-lambda (head tail)
			      (make-node 'core-do
					 'head head
					 'tail tail)))))

	     (patterns (/ ((ps <- pattern)* (! #\,) ,(packrat-lambda (ps) ps))
			  (p <- tuple-pattern ,(packrat-lambda (p) (list p)))))

	     (pattern (/ message-pattern
			 stream-pattern
			 (#\_ ws ,(packrat-lambda () (make-node 'pat-discard)))
			 (l <- literal ,(packrat-lambda (l) (make-node 'pat-lit 'value l)))
			 (q <- qname ,(packrat-lambda (q) (make-node 'pat-binding 'name q)))
			 (OPAREN p <- tuple-pattern CPAREN ,(packrat-lambda (p) p))))

	     (tuple-pattern (ps <- comma-separated-patterns
				,(packrat-lambda (ps)
				   (if (and (pair? ps)
					    (null? (cdr ps)))
				       (car ps)
				       (make-node 'pat-tuple 'elements ps)))))

	     (message-pattern (OANGLE (ps <- message-pattern-component)* CANGLE
				      ,(packrat-lambda (ps) (make-node 'pat-message 'parts ps))))

	     (message-pattern-component ((! #\>) pattern))

	     (stream-pattern (OBRACK p <- comma-separated-patterns s <- stream-pattern-suffix
				     ,(packrat-lambda (p s) (expand-stream-pattern p s))))

	     (stream-pattern-suffix (/ (CBRACK ,(packrat-lambda () #f))
				       (PIPE CBRACK ,(packrat-lambda () #f))
				       (PIPE p <- pattern CBRACK ,(packrat-lambda (p) p))))

	     (comma-separated-patterns (/ (p <- pattern (#\, ws ps <- pattern)*
					     ,(packrat-lambda (p ps) (cons p ps)))
					  ,(packrat-lambda () '())))

	     (sexp (/ (#\. ws s <- sexp ,(packrat-lambda (s) (list 'quote s)))
		      id
		      literal
		      string
		      (OPAREN (s <- sexp)* CPAREN ,(packrat-lambda (s) s))))

	     ;;---------------------------------------------------------------------------

	     (semis (SEMI *))

	     (qname (/ qname-nooperator
		       (OPARENnows o <- operator CPAREN ,(packrat-lambda (o)
							   (make-qname #f o)))))

	     (qname-nooperator
	      (/ (prefix <- id #\: localname <- id
			 ,(packrat-lambda (prefix localname)
			    (make-qname prefix localname)))
		 (uri <- string #\: localname <- id
		      ,(packrat-lambda (uri localname)
			 (make-qname uri localname)))
		 (#\: localname <- id
		  ,(packrat-lambda (localname)
		     (make-qname (string->symbol "") localname)))
		 (localname <- id
			    ,(packrat-lambda (localname)
			       (make-qname #f localname)))))

	     (id ((! #\_) (a <- id-alpha) (r <- (/ id-alpha digit))* ws
		  ,(packrat-lambda (a r) (string->symbol (list->string (cons a r))))))

	     (string (#\' (cs <- (/: ,non-string-quote "string character"))* #\' ws
		      ,(packrat-lambda (cs) (list->string cs))))

	     (operator ((! reserved-operator)
			a <- op-punct-init (r <- (/ op-punct digit alpha))* ws
			,(packrat-lambda (a r) (string->symbol (list->string (cons a r))))))

	     (word (/ positive-word
		      (#\- ws w <- positive-word ,(packrat-lambda (w) (- w)))))
	     (positive-word ((d <- digit)+ ws
			     ,(packrat-lambda (d) (string->number (list->string d)))))

	     (id-alpha (/ alpha #\_ #\$))
	     (op-punct      (/: "!#%&*+/<=>?@\\^-~:|"))
	     (op-punct-init (/: "!#%&*+/<=>?@\\^-~"))

	     (ws (/ ((/: ,char-whitespace? "whitespace")+ ws)
		    (#\" (/: ,nonquote "comment character")* #\" ws)
		    ()))
	     (digit (/: ,char-numeric? "digit"))
	     (alpha (/: ,char-alphabetic? "letter"))

	     (reserved-operator (/ ARROW
				   COLONEQ
				   EQ
				   PIPE))

	     (ARROW ("->" (! op-punct) ws))
	     (COLONEQ (":=" (! op-punct) ws))
	     (EQ (#\= (! op-punct) ws))

	     (SEMI (#\; ws))
	     (OPAREN (OPARENnows ws))
	     (OPARENnows #\()
	     (CPAREN (#\) ws))
	     (OBRACK (#\[ ws))
	     (CBRACK (#\] ws))
	     (OANGLE (#\< ws))
	     (CANGLE (#\> ws))
	     (OBRACE (#\{ ws))
	     (CBRACE (#\} ws))
	     (PIPE (#\| ws))

	     (DEFINE ("define"ws))
	     (NAMESPACE ("namespace"ws))
	     (SELF ("self"ws))
	     (NEXTMETHOD ("nextMethod"ws))
	     (LET ("let"ws))
	     (DO ("do"ws))
	     (META ("meta"ws))

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
