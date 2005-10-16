(define (port-results filename p)
  (base-generator->results
   (let ((ateof #f)
	 (pos (top-parse-position filename)))
     (lambda ()
       (if ateof
	   (values pos #f)
	   (let ((x (read-char p)))
	     (if (eof-object? x)
		 (begin
		   (set! ateof #t)
		   (values pos #f))
		 (let ((old-pos pos))
		   (set! pos (update-parse-position pos x))
		   (values old-pos (cons x x))))))))))

(define (string-results filename s)
  (base-generator->results
   (let ((idx 0)
	 (len (string-length s))
	 (pos (top-parse-position filename)))
     (lambda ()
       (if (= idx len)
	   (values pos #f)
	   (let ((x (string-ref s idx))
		 (old-pos pos))
	     (set! pos (update-parse-position pos x))
	     (set! idx (+ idx 1))
	     (values old-pos (cons x x))))))))

(define (parse-result->value error-text result)
  (if (parse-result-successful? result)
      (parse-result-semantic-value result)
      (error error-text
	     (let ((e (parse-result-error result)))
	       (list error-text
		     (parse-position->string (parse-error-position e))
		     (parse-error-expected e)
		     (parse-error-messages e))))))

(define (packrat-token str)
  (lambda (starting-results)
    (let loop ((pos 0) (results starting-results))
      (if (= pos (string-length str))
	  (make-result str results)
	  (if (and results (char=? (parse-results-token-value results) (string-ref str pos)))
	      (loop (+ pos 1) (parse-results-next results))
	      (make-expected-result (parse-results-position starting-results) str))))))

(define (parse-results-take results n)
  (let loop ((acc '())
	     (results results)
	     (n n))
    (if (zero? n)
	(values (list->string (reverse acc))
		results)
	(loop (cons (parse-results-token-value results) acc)
	      (parse-results-next results)
	      (- n 1)))))

(define (parse-results->pregexp-stream results)
  (pregexp-make-stream (lambda (r)
			 (if r
			     (cons (parse-results-token-value r)
				   (parse-results-next r))
			     (cons #f #f)))
		       results))

(define (packrat-regex name . string-fragments)
  (let* ((exp (string-concatenate string-fragments))
	 (re (pregexp exp)))
    (lambda (results)
      (let* ((stream (parse-results->pregexp-stream results))
	     (match (pregexp-match-head re stream)))
	(if match
	    (let-values (((str next) (parse-results-take results (cdar match))))
	      (make-result str next))
	    (make-expected-result (parse-results-position results) name))))))

(define (packrat-cache key parser)
  (lambda (results)
    (results->result results key
		     (lambda ()
		       (parser results)))))

(define-syntax define-packrat-cached
  (syntax-rules ()
    ((_ (fnname results) body ...)
     (define fnname
       (packrat-cache 'fnname
		      (letrec ((fnname (lambda (results) body ...)))
			fnname))))
    ((_ fnname exp)
     (define fnname
       (packrat-cache 'fnname exp)))))

(define-values (parse-ThiNG parse-ThiNG-toplevel)
  (let* ((p "[-+=_|/?.<>*&^%$#@!`~]")
	 (midsym (string-append "([a-zA-Z0-9]|"p")")))
    (packrat-parser (begin
		      (define-packrat-cached (white results)
			(if (and-let* ((ch (parse-results-token-value results)))
			      (char-whitespace? ch))
			    (white (parse-results-next results))
			    (comment results)))
		      (define-packrat-cached (comment results)
			(if (eq? (parse-results-token-value results) #\")
			    (skip-comment-body (parse-results-next results))
			    (make-result 'whitespace results)))
		      (define (skip-comment-body results)
			(if (eq? (parse-results-token-value results) #\")
			    (white (parse-results-next results))
			    (skip-comment-body (parse-results-next results))))
		      (define (string-body results)
			(string-body* results '()))
		      (define (string-body* results acc)
			(let ((ch (parse-results-token-value results))
			      (next (parse-results-next results)))
			  (if (eq? ch #\')
			      (string-body-quote next acc)
			      (string-body* next (cons ch acc)))))
		      (define (string-body-quote results acc)
			(if (eq? (parse-results-token-value results) #\')
			    (string-body* (parse-results-next results) (cons #\' acc))
			    (make-result (list->string (reverse acc)) results)))
		      (define-packrat-cached atom (packrat-regex 'atom "[A-Z]"midsym"*"))
		      (define-packrat-cached var (packrat-regex 'var "[a-z]"midsym"*"))
		      (define-packrat-cached infixop-raw (packrat-regex 'infixop p midsym"*"))
		      (define-packrat-cached integer (packrat-regex 'integer "[0-9]+"))
		      (define (make-binary op left right)
			`(adj ,op (adj ,left (adj ,right (tuple)))))
		      (define (rewrite-infix parts)
			(let loop ((left (second parts))
				   (parts parts))
			  (let* ((at-end (car parts)) (parts (cdr parts))
						      (parts (cdr parts))
				 (op (car parts))     (parts (cdr parts))
				 (rest (car parts)))
			  (if at-end
			      (make-binary op left rest)
			      (loop (make-binary op left (second rest))
				    rest)))))
		      (values tuple1 toplevel))
		    (toplevel ((d <- tuple1 white '#\; '#\;) d)
			      ((white '#f) `(atom |Quit|)))
		    (datum ((s <- tuple0) s))
		    (tuple0 ((s <- tuple1) s)
			    (() '(tuple)))
		    (tuple1 ((s <- tuple1*) (if (= (length s) 2) (cadr s) s)))
		    (tuple1* ((d <- fun white '#\, s <- tuple1*) `(tuple ,d ,@(cdr s)))
			     ((d <- fun) `(tuple ,d)))
		    (fun ((f <- fun*) f)
			 ((v <- funcall f <- fun*) `(adj ,v (adj (quote ,f) (tuple))))
			 ((v <- funcall) v))
		    (fun* ((e <- entry white d <- fun*) `(fun ,e ,@(cdr d)))
			  ((e <- entry) `(fun ,e)))
		    (entry ((k <- simple colon v <- funcall) (list k v)))
		    (semi ((white '#\; (! '#\;)) 'semi))
		    (colon ((white '#\:) 'colon))
		    (funcall ((parts <- funcall*) (rewrite-infix parts))
			     ((a <- adj) a))
		    (funcall* ((a <- adj o <- infixop b <- funcall*) (list #f a o b))
			      ((a <- adj o <- infixop b <- adj) (list #t a o b)))
		    (infixop ((white r <- infixop-raw) `(var ,(string->symbol r))))
		    (adj ((v <- simple white vs <- adj2) `(adj ,v ,vs))
			 ((v <- simple semi vs <- simple) `(adj ,v ,vs))
			 ((v <- simple (! colon)) v))
		    (adj2 ((v <- simple white vs <- adj2) `(adj ,v ,vs))
			  ((v <- simple semi vs <- simple) `(adj ,v ,vs))
			  ((v <- simple (! colon)) `(adj ,v (tuple))))
		    (simple ((white d1 <- simple1) d1))
		    (simple1 (('#\( o <- infixop white '#\)) o)
			     (('#\( d <- datum white '#\)) d)
			     (('#\[ d <- datum white '#\]) `(quote ,d))
			     (('#\{ d <- datum white '#\}) `(meta-quote ,d))
			     ((l <- literal) `(lit ,l))
			     ((a <- var) `(var ,(string->symbol a)))
			     ((a <- atom) `(atom ,(string->symbol a)))
			     (('#\' s <- string-body) `(atom ,(string->symbol s)))
			     (('#\_) `(discard)))
		    (literal ((i <- integer) (string->number i))
			     (('#\- i <- integer) (- (string->number i)))))))

(define read-ThiNG
  (lambda ()
    (parse-result->value "While parsing ThiNG"
			 (parse-ThiNG-toplevel (port-results "stdin" (current-input-port))))))

(define string->ThiNG
  (lambda (s)
    (parse-result->value "While parsing ThiNG"
			 (parse-ThiNG (string-results "<string>" s)))))
