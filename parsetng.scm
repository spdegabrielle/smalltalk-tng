(require 'srfi-1) ; list
(require 'srfi-13) ; string
(require 'srfi-14) ; charset

(eval-when (compile) (load "packrat.scm"))
(require 'util)
(require 'packrat)

;---------------------------------------------------------------------------
;; utilities

(define (transform-grammar grammar)
  (map (lambda (clause)
	 (let ((v (last clause))
	       (front (butlast clause)))
	   (if (procedure? v)
	       clause
	       (append front (list (lambda args
				     (debug 3 'reducing clause args)
				     (let walk ((formal v))
				       (cond
					((null? formal) '())
					((pair? formal) (cons (walk (car formal))
							      (walk (cdr formal))))
					((procedure? formal) (apply formal args))
					((number? formal) (list-ref args formal))
					(else formal)))))))))
       grammar))

;---------------------------------------------------------------------------
;; lex0: categorising characters

(define (lex0-ThiNG char-provider-thunk)
  (let ((char (char-provider-thunk)))
    (and (not (eof-object? char))
	 (cons (cond
		((char-set-contains? char-set:letter char) 'letter)
		((char-set-contains? char-set:digit char) 'digit)
		((or (char-whitespace? char)
		     (char-set-contains? char-set:blank char))
		 'whitespace)
		(else (case char
			((#\() 'oparen)
			((#\)) 'cparen)
			((#\[) 'obrack)
			((#\]) 'cbrack)
			((#\{) 'obrace)
			((#\}) 'cbrace)
			((#\+) 'plus)
			((#\-) 'minus)
			((#\=) 'equal)
			((#\") 'doublequote)
			((#\') 'quote)
			((#\.) 'dot)
			((#\:) 'colon)
			((#\|) 'pipe)
			((#\@) 'at)
			((#\#) 'hash)
			((#\\) 'backslash)
			((#\_) 'underscore)
			((#\*) 'star)
			(else 'misc))))
	       char))))

(define (unfold-lex0-ThiNG port)
  (unfold (lambda (dummy) (eof-object? (peek-char port)))
	  lex0-ThiNG
	  (lambda (token) token)
	  (lambda () (read-char port))))

;---------------------------------------------------------------------------
;; lex1: building tokens from categorised character stream
;;
;; compound (pseudo-)token kinds:
;;   identifier   [a-zA-Z][a-zA-Z0-9]*:?
;;   symbol       [^ ]+
;;   integer      [-+]?[0-9]+/[^.]
;;   comment      "([^"\\]|\\"|\\\\])*"
;;   string       '([^'\\]|\\'|\\\\])*'
;;   :=

(define (make-lex1-ThiNG filename char-provider-thunk)
  (let* ((pushback* '())
	 (position (top-parse-position filename))
	 (prev-position position))
    (define (next!)
      (if (null? pushback*)
	  (let* ((newval (lex0-ThiNG char-provider-thunk)))
	    (if newval
		(begin
		  (set! prev-position position)
		  (set! position (update-parse-position position (cdr newval)))))
	    newval)
	  (let ((v (car pushback*)))
	    (set! pushback* (cdr pushback*))
	    (set! prev-position position)
	    (set! position (cdr v))
	    (car v))))

    (define (pushback! x)
      (set! pushback* (cons (cons x position) pushback*))
      (set! position prev-position))

    (define (go fn . data)
      (dispatch* (next!) fn data))

    (define (dispatch token fn . data)
      (dispatch* token fn data))

    (define emit-k 'emit-k)

    (define (dispatch* token fn data)
      (if token
	  (apply fn token (car token) (cdr token) data)
	  (emit-k #f)))

    (define (emit kind sv)
      (emit-k (cons prev-position (cons kind sv))))

    (define (lex token kind sv)
      (case kind
	((whitespace) (go lex))
	((minus plus) (go lex-sign token))
	((digit) (pushback! token) (go lex-number #f 0))
	((letter) (go lex-identifier (list sv)))
	((colon) (go lex-colon token))
	((doublequote) (go lex-string token '() (lambda (result) (go lex))))
	((quote) (go lex-string token '() (lambda (result) (emit 'string result))))
	((hash) (go lex-symbol '()))
	((misc equal star) (go lex-punct (list sv)))
	(else (emit kind sv))))

    (define (lex-sign token kind sv sign-token)
      (pushback! token)
      (if (eq? kind 'digit)
	  (go lex-number (car sign-token) 0)
	  (go lex-punct (list (cdr sign-token)))))

    (define (lex-punct token kind sv acc)
      (case kind
	((misc equal star plus minus) (go lex-punct (cons sv acc)))
	(else
	 (pushback! token)
	 (emit 'punct (list->string (reverse acc))))))

    (define (lex-number token kind sv sign acc)
      (case kind
	((digit) (go lex-number sign (+ (* acc 10)
					(- (char->integer sv)
					   (char->integer #\0)))))
	((dot) (go lex-decimal sign acc token))
	(else
	 (pushback! token)
	 (finish-integer sign acc))))

    (define (lex-decimal token kind sv sign acc dot-token)
      (case kind
	((digit) (error "Illegal syntax - floating-point literals not supported"))
	(else
	 (pushback! token)
	 (pushback! dot-token)
	 (finish-integer sign acc))))

    (define (finish-integer sign acc)
      (emit 'integer (* (if (eq? sign 'minus) -1 1) acc)))

    (define (lex-identifier token kind sv acc)
      (case kind
	((letter digit) (go lex-identifier (cons sv acc)))
	((colon) (go lex-selector-identifier token acc))
	(else
	 (pushback! token)
	 (finish-identifier 'identifier acc))))

    (define (lex-selector-identifier token kind sv colon-token acc)
      (pushback! token)
      (if (memq kind '(equal star))
	  (begin
	    (pushback! colon-token)
	    (finish-identifier 'identifier acc))
	  (finish-identifier 'selector (cons #\: acc))))

    (define (lex-symbol token kind sv acc)
      (case kind
	((letter digit misc equal star plus minus) (go lex-symbol (cons sv acc)))
	(else
	 (pushback! token)
	 (finish-identifier 'symbol acc))))

    (define (finish-identifier kind acc)
      (let ((idstr (list->string (reverse acc))))
	(if (and (eq? kind 'identifier)
		 (string=? idstr "resend"))
	    (emit 'resend 'resend)
	    (emit kind idstr))))

    (define (lex-colon token kind sv colon-token)
      (case kind
	((equal) (emit 'colonequal #f))
	((star) (emit 'colonstar #f))
	(else
	 (pushback! token)
	 (emit 'colon (cdr colon-token)))))

    (define (lex-string token kind sv terminator acc k)
      (if (eq? kind (car terminator))
	  (k (list->string (reverse acc)))
	  (case kind
	    ((backslash) (go (lambda (token2 kind2 sv2)
			       (go lex-string 
				   terminator
				   (cons (case sv2
					   ((#\n) #\newline)
					   ((#\t) #\tab)
					   (else sv2))
					 acc)
				   k))))
	    (else (go lex-string terminator (cons sv acc) k)))))

    (lambda ()
      (call-with-current-continuation
       (lambda (k)
	 (set! emit-k k)
	 (error "Value returned without emit from lexer" (go lex)))))))

(define (unfold-lex1-ThiNG port)
  (let ((lexer (make-lex1-ThiNG (lambda () (read-char port))))
	(done #f))
    (unfold (lambda (dummy) done)
	    (lambda (dummy)
	      (let ((result (lexer)))
		(if (not result)
		    (set! done #t))
		result))
	    (lambda (token) token)
	    'dummy1)))

;---------------------------------------------------------------------------
;; parsing

(define (fixup-nary first-val args)
  (let* ((selectors (map car args))
	 (vals (map cadr args))
	 (selector (string-concatenate selectors)))
    `(send ,selector ,(cons first-val vals))))

(define-values (ThiNG-parser ThiNG-topexpr-parser)
  (packrat-parser
   (values toplevel topexpr)

   (toplevel ((a <- topexpr 'dot b <- toplevel) (cons a b))
	     ((a <- topexpr 'dot '#f) (list a))
	     ((a <- topexpr '#f) (list a)))

   (topexpr ((a <- method-definition) a)
	    ((a <- expr) a))

   (expr ((a <- nary) a))

   (nary ((a <- binary args <- nary-args) (fixup-nary a args))
	 ((a <- binary) a))

   (nary-args ((sel <- selector b <- binary rest <- nary-args) (cons (list sel b) rest))
	      ((sel <- selector b <- binary) (list (list sel b))))

   (binary ((u1 <- unary k <- binaryk) (k u1)))
   (binaryk ((op <- binaryop u2 <- unary k <- binaryk)
	     (lambda (u1) (k `(send ,op (,u1 ,u2)))))
	    (()
	     (lambda (u1) u1)))

   (binaryop ((p <- 'punct) p))

   (unary ((v <- value k <- unaryk) (k v)))
   (unaryk ((i <- id (! (/ ('colonequal) ('colonstar))) k <- unaryk)
	    (lambda (v) (k `(send ,i (,v)))))
	   (()
	    (lambda (v) v)))

   (value ((i <- id 'oparen s <- stmt-seq 'cparen) `(scope ,i ,s))
	  ((i <- id) `(ref ,i))
	  ((b <- block) `(block . ,b))
	  ((s <- 'string) `(string ,s))
	  ((s <- 'symbol) `(symbol ,(string->symbol s)))
	  ((i <- 'integer) `(number ,i))
	  (('resend) `(resend))
	  (('oparen e <- expr u <- updates+ 'cparen) `(update ,e ,u))
	  (('oparen u <- updates 'cparen) `(update (ref "Root") ,u))
	  (('oparen e <- expr 'cparen) e)
	  (('oparen s <- stmt-seq 'cparen) `(scope ,*nil* ,s))
	  (('obrace ee <- expr-seq 'cbrace) `(tuple ,ee)))

   (updates+ ((u <- update uu <- updates) (cons u uu)))
   (updates ((u <- update uu <- updates) (cons u uu))
	    (() '()))

   (update ((i <- id 'colonequal e <- expr) (list *false* i e))
	   ((i <- id 'colonstar e <- expr) (list *true* i e)))

   (block (('obrack b <- binders s <- stmt-seq 'cbrack) (list b s)))

   (expr-seq ((e <- expr 'dot ee <- expr-seq) (cons e ee))
	     ((e <- expr) (list e))
	     (() '()))

   (stmt-seq ((e <- stmt 'dot s <- stmt-seq) (cons e s))
	     ((e <- stmt) (list e))
	     (() '()))

   (stmt ((i <- id 'colonequal e <- expr) `(let ,i ,e))
	 ((e <- expr) e))

   (binders ((b <- binders+ 'pipe) b)
	    (() '()))
   (binders+ ((b <- binder bb <- binders+) (cons b bb))
	     ((b <- binder) (list b)))
   (binder (('colon i <- id) i))

   (method-definition ((p <- method-params 'obrack ee <- stmt-seq 'cbrack) `(method ,p ,ee)))

   (method-params ((p1 <- method-param op <- binaryop p2 <- method-param) `(send ,op (,p1 ,p2)))
		  ((p1 <- method-param n <- method-nary) (fixup-nary p1 n))
		  ((p <- method-param i <- id) `(send ,i (,p))))

   (method-param (('underscore 'at v <- value) (list *false* v))
		 (('underscore) (list *false* *false*))
		 ((i <- id 'at v <- value) (list i v))
		 ((i <- id) (list i *false*)))

   (method-nary ((s <- selector p <- method-param r <- method-nary) (cons (list s p) r))
		((s <- selector p <- method-param) (list (list s p))))

   (selector ((s <- 'selector) s))
   (id ((i <- 'identifier) i))))

(define (parse-ThiNG filename parser char-provider-thunk)
  (let* ((lexer (make-lex1-ThiNG filename char-provider-thunk))
	 (result (parser (base-generator->results
			  (lambda ()
			    (let ((r (lexer)))
			      (if r
				  (values (car r) (cdr r))
				  (values #f #f))))))))
    (if (parse-result-successful? result)
	(values #t (parse-result-semantic-value result))
	(let ((e (parse-result-error result)))
	  (values #f (list 'parse-error
			   (parse-position->string (parse-error-position e))
			   (parse-error-expected-strings e)
			   (parse-error-messages e)))))))
