#lang racket/base

(require rackunit)

(struct operative (formals envformal body staticenv) #:transparent)
(struct applicative (underlying) #:transparent)
(struct primitive (underlying) #:transparent)

(define (vau-eval exp env k)
  (cond
   ((symbol? exp) (k (lookup exp env)))
   ((not (pair? exp)) (k exp))
   (else (vau-eval (car exp) env (make-combiner (cdr exp) env k)))))

(define (make-combiner argtree env k)
  (lambda (rator)
    (cond
     ((procedure? rator)
      (vau-eval-args argtree '() env (make-primitive-applier rator k)))
     ((operative? rator)
      (vau-eval (operative-body rator)
		(extend (operative-staticenv rator)
			(bind! (vau-match (operative-formals rator)
					  argtree)
			       (operative-envformal rator)
			       env))
		k))
     ((applicative? rator)
      (vau-eval-args argtree
		     '()
		     env
		     (make-applicative-combiner (applicative-underlying rator) env k)))
     ((primitive? rator)
      (apply (primitive-underlying rator) k env argtree))
     (else (vau-error 'vau-eval "Not a callable: ~v with argtree: ~v" rator argtree)))))

(define (make-primitive-applier rator k)
  (lambda (args)
    (k (apply rator args))))

(define (make-applicative-combiner op env k)
  (lambda (args)
    (vau-eval (cons op args) env k)))

(define (vau-eval-args args revacc env k)
  (if (null? args)
      (k (reverse revacc))
      (vau-eval (car args) env
		(make-args-evaler (cdr args) revacc env k))))

(define (make-args-evaler remainder revacc env k)
  (lambda (v)
    (vau-eval-args remainder (cons v revacc) env k)))

(define (vau-error . args)
  (apply error args))

(define (lookup name env)
  (if (null? env)
      (vau-error 'vau-eval "Variable not found: ~v" name)
      (hash-ref (car env) name (lambda () (lookup name (cdr env))))))

(define (extend env rib)
  (cons rib env))

(define (bind! rib name value)
  (if (eq? name '#:ignore)
      rib
      (begin (hash-set! rib name value)
	     rib)))

(define (empty-env)
  '())

(define (empty-rib)
  (make-hash))

(define (vau-match p v)
  (let m ((p p)
	  (v v)
	  (rib (empty-rib)))
    (cond
     ((pair? p) (if (pair? v)
		    (m (cdr p) (cdr v) (m (car p) (car v) rib))
		    (vau-error 'vau-match "Expected a pair matching ~v; got ~v" p v)))
     ((symbol? p) (bind! rib p v))
     ((eq? p '#:ignore) rib)
     (else (if (eqv? p v)
	       rib
	       (vau-error 'vau-match "Expected a literal eqv? to ~v; got ~v" p v))))))

(define (vau-lexer [port (current-input-port)])
  (let ()
    (define (eat!) (read-char port))
    (define (emit val) (eat!) val)
    (define (emit-number acc)
      (string->number (list->string (reverse acc))))
    (define (go state . args)
      (eat!)
      (apply state (peek-char port) args))
    (define (keywordize str)
      (cond
       ((string=? str "t") #t)
       ((string=? str "f") #f)
       (else (string->keyword str))))
    (define (start c)
      (cond
       ((eof-object? c) (emit c))
       ((char-whitespace? c) (go start))
       ((char-numeric? c) (go number (list c)))
       (else (case c
	       ((#\;) (go line-comment))
	       ((#\-) (go negsign))
	       ((#\") (go string (list)))
	       ((#\#) (go symbol (list) keywordize))
	       ((#\() (emit #\())
	       ((#\)) (emit #\)))
	       (else (go symbol (list c) string->symbol))))))
    (define (line-comment c)
      (case c
	((#\return #\newline) (go start))
	(else (go line-comment))))
    (define (negsign c)
      (cond
       ((and (char? c) (char-numeric? c)) (number c (list #\-)))
       (else (symbol c (list #\-) string->symbol))))
    (define (number c acc)
      (cond
       ((eof-object? c) (emit-number acc))
       ((char-numeric? c) (go number (cons c acc)))
       ((eqv? c #\.) (go fraction (cons c acc)))
       (else (exponent c acc))))
    (define (fraction c acc)
      (cond
       ((eof-object? c) (emit-number acc))
       ((char-numeric? c) (go fraction (cons c acc)))
       (else (exponent c acc))))
    (define (exponent c acc)
      (case c
	((#\e #\E) (go expsign (cons c acc)))
	(else (emit-number acc))))
    (define (expsign c acc)
      (case c
	((#\- #\+) (go expdigits (cons c acc)))
	(else (cond
	       ((eof-object? c) (emit-number acc))
	       ((char-numeric? c) (go expdigits (cons c acc)))
	       (else (vau-error 'vau-lexer
				"Syntax error: expected at least one digit in exponent"))))))
    (define (expdigits c acc)
      (cond
       ((eof-object? c) (emit-number acc))
       ((char-numeric? c) (go expdigits (cons c acc)))
       (else (emit-number acc))))
    (define (string c acc)
      (case c
	((#\") (emit (list->string (reverse acc))))
	((#\\) (go string-escape acc))
	(else (if (eof-object? c)
		  (vau-error 'vau-lexer "Syntax error: unterminated string")
		  (go string (cons c acc))))))
    (define (string-escape c acc)
      (case c
	((#\\ #\") (go string (cons c acc)))
	(else (vau-error 'vau-lexer "Syntax error: bad escape in string: ~v" c))))
    (define (symbol c acc finalize)
      (cond
       ((eof-object? c) (emit (finalize (list->string (reverse acc)))))
       ((char-alphabetic? c) (go symbol (cons c acc) finalize))
       ((char-numeric? c) (go symbol (cons c acc) finalize))
       ((memv c '(#\- #\! #\@ #\$ #\% #\^ #\& #\* #\_ #\= #\+ #\: #\< #\> #\/ #\?))
	(go symbol (cons c acc) finalize))
       (else (finalize (list->string (reverse acc))))))
    (lambda () (start (peek-char port)))))

(define (vau-read [port (current-input-port)])
  (let ((next (vau-lexer port)))
    (define (read)
      (read* (next)))
    (define (read* v)
      (cond
       ((eqv? v #\() (read-list '()))
       ((eqv? v #\)) (vau-error 'vau-read "Syntax error: unexpected close paren"))
       (else v)))
    (define (read-list acc)
      (let ((v (next)))
	(cond
	 ((eof-object? v) (vau-error 'vau-read "End-of-file in unterminated list"))
	 ((eqv? v #\)) (reverse acc))
	 (else (read-list (cons (read* v) acc))))))
    (read)))

(define (lex-all s)
  (let ((next (vau-lexer (open-input-string s))))
    (let loop ()
      (let ((v (next)))
	(if (eof-object? v)
	    '()
	    (cons v (loop)))))))

(check-equal? (lex-all "abc") '(abc))
(check-equal? (lex-all "\"abc\"") '("abc"))
(check-equal? (lex-all "\"ab\\\"c\"") '("ab\"c"))
(check-equal? (lex-all "abc 123e-2 -2.34e2 12 -12") '(abc 1.23 -234.0 12 -12))
(check-equal? (lex-all "(abc()( abc ) abc (#abc) ()abc)")
	      '(#\( abc #\( #\) #\( abc #\) abc #\( #:abc #\) #\( #\) abc #\) ))

(check-equal? (vau-read (open-input-string "(abc()( abc ) abc 12e0z (#abc) ()abc)"))
	      '(abc () (abc) abc 12.0 z (#:abc) () abc))

;---------------------------------------------------------------------------

(define (alist->rib xs)
  (let ((rib (empty-rib)))
    (for-each (lambda (entry)
		(bind! rib (car entry) (cadr entry)))
	      xs)
    rib))

;---------------------------------------------------------------------------

(define $begin
  (primitive (lambda (k dynenv . exps)
	       (if (null? exps)
		   (k (void))
		   (let loop ((exps exps))
		     (vau-eval (car exps) dynenv
			       (if (null? (cdr exps))
				   k
				   (lambda (v) (loop (cdr exps))))))))))

(define $vau
  (primitive (lambda (k dynenv formals envformal . exps)
	       (let ((body (cond
			    ((null? exps) (void))
			    ((null? (cdr exps)) (car exps))
			    (else (cons $begin exps)))))
		 (k (operative formals envformal body dynenv))))))

(define coreenv
  (extend (empty-env)
	  (alist->rib
	   `((eval ,(lambda (exp env) (vau-eval exp env values)))
	     (list* ,list*)
	     ($define! ,(primitive (lambda (k dynenv name valexp)
				     (when (not (symbol? name))
				       (vau-error '$define! "Needs symbol name; got ~v" name))
				     (vau-eval valexp dynenv
				       (lambda (value)
					 ;; TODO: destructuring-bind definitions
					 (bind! (car dynenv) name value)
					 (k value))))))
	     ($begin ,$begin)
	     ($vau ,$vau)
	     (wrap ,applicative)
	     (unwrap ,applicative-underlying)
	     ;; (unwrap ,(lambda (x)
	     ;; 		(cond
	     ;; 		 ((applicative? x) (applicative-underlying x))
	     ;; 		 ((procedure? x) (primitive (lambda (k dynenv . args)
	     ;; 					      (k (apply x args))))))))
	     ))))

(define $lambda
  (vau-eval `($define! $lambda
		       ($vau (formals . exps) dynenv
			     (wrap (eval (list* $vau formals #:ignore exps) dynenv))))
	    coreenv
	    values))

;---------------------------------------------------------------------------

(define baseenv
  (extend coreenv
	  (alist->rib
	   `(($if ,(primitive (lambda (k dynenv test true false)
				(vau-eval test dynenv
				  (lambda (result)
				    (case result
				      ((#t) (vau-eval true dynenv k))
				      ((#f) (vau-eval false dynenv k))
				      (else (vau-error '$if "Test must evaluate to boolean"))))))))
	     ($let ,(primitive (lambda (k dynenv bindings . exps)
				 (vau-eval (list* (list* $lambda (map car bindings) exps)
						  (map cadr bindings))
					   dynenv
					   k))))

	     ;; (make-environment ,(lambda parents
	     ;; 			  (extend (apply append parents) (empty-rib))))
	     ;; (operative? ,(lambda (x)
	     ;; 		    (or (operative? x)
	     ;; 			(primitive? x))))
	     ;; (applicative? ,(lambda (x)
	     ;; 		      (or (applicative? x)
	     ;; 			  (procedure? x))))

	     (open-input-file ,open-input-file)
	     (close-input-port ,close-input-port)
	     (eof-object? ,eof-object?)

	     (read ,vau-read)

	     (write ,write)
	     (display ,display)
	     (newline ,newline)
	     (flush-output ,flush-output)
	     (format ,format)

	     (box ,box)
	     (unbox ,unbox)
	     (set-box! ,set-box!)

	     (+ ,+) (- ,-) (* ,*) (/ ,/)
	     (quotient ,quotient) (remainder ,remainder) (modulo ,modulo)
	     (number->string ,number->string) (string->number ,string->number)

	     (cons ,cons)
	     (car ,car)
	     (cdr ,cdr)
	     (pair? ,pair?)
	     (null? ,null?)

	     (symbol? ,symbol?)

	     (eq? ,eq?)
	     (eqv? ,eqv?)
	     (not ,not)
	     (hash->list ,hash->list)

	     ))))

(define $load!
  (vau-eval `($define! $load!
		       ($vau (filename-exp) dynenv
			     ($define! filename (eval filename-exp dynenv))
			     ($define! p (open-input-file filename))
			     ($define! loop
				       ($lambda ()
						($let ((exp (read p)))
						      ($if (eof-object? exp)
							   ($begin (close-input-port p)
								   #:ignore)
							   ($begin (eval exp dynenv)
								   (loop))))))
			     (loop)))
	    baseenv
	    values))

(let () ;; new scope to suppress toplevel expr result printing
  (vau-eval `($load! "prelude.vau") baseenv values)
  (void))
