#lang racket/base

(require rackunit)

(struct operative (formals envformal body staticenv) #:transparent)
(struct applicative (underlying) #:transparent)
(struct primitive (underlying) #:transparent)

(define (vau-eval exp env)
  (let v ((exp exp))
    (cond
     ((symbol? exp) (lookup exp env))
     ((not (pair? exp)) exp)
     (else (let ((rator (v (car exp))))
	     (cond
	      ((procedure? rator) (apply rator (map v (cdr exp))))
	      ((operative? rator) (vau-eval (operative-body rator)
					    (extend (operative-staticenv rator)
						    (bind! (vau-match (operative-formals rator)
								      (cdr exp))
							   (operative-envformal rator)
							   env))))
	      ((applicative? rator) (v (cons (applicative-underlying rator)
					     (map v (cdr exp)))))
	      ((primitive? rator) (apply (primitive-underlying rator) (cons env (cdr exp))))
	      (else (vau-error 'vau-eval "Not a callable: ~v in exp: ~v" rator exp))))))))

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
  (primitive (lambda (dynenv . exps)
	       (if (null? exps)
		   (void)
		   (let loop ((exps exps))
		     (if (null? (cdr exps))
			 (vau-eval (car exps) dynenv)
			 (begin (vau-eval (car exps) dynenv)
				(loop (cdr exps)))))))))

(define $vau
  (primitive (lambda (dynenv formals envformal . exps)
	       (let ((body (cond
			    ((null? exps) (void))
			    ((null? (cdr exps)) (car exps))
			    (else (cons $begin exps)))))
		 (operative formals envformal body dynenv)))))

(define coreenv
  (extend (empty-env)
	  (alist->rib
	   `((eval ,vau-eval)
	     (list* ,list*)
	     ($define! ,(primitive (lambda (dynenv name valexp)
				     (let ((value (vau-eval valexp dynenv)))
				       (when (not (symbol? name))
					 (vau-error '$define! "Needs symbol name; got ~v" name))
				       ;; TODO: destructuring-bind definitions
				       (bind! (car dynenv) name value)
				       value))))
	     ($begin ,$begin)
	     ($vau ,$vau)
	     (wrap ,applicative)
	     (unwrap ,applicative-underlying)))))

(define $lambda
  (vau-eval `($define! $lambda
		       ($vau (formals . exps) dynenv
			     (wrap (eval (list* $vau formals #:ignore exps) dynenv))))
	    coreenv))

;---------------------------------------------------------------------------

(define baseenv
  (extend coreenv
	  (alist->rib
	   `(($if ,(primitive (lambda (dynenv test true false)
				(case (vau-eval test dynenv)
				  ((#t) (vau-eval true dynenv))
				  ((#f) (vau-eval false dynenv))
				  (else (vau-error '$if "Test must evaluate to boolean"))))))
	     ($let ,(primitive (lambda (dynenv bindings . exps)
				 (vau-eval (list* (list* $lambda (map car bindings) exps)
						  (map cadr bindings))
					   dynenv))))

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

	     (eq? ,eq?)
	     (eqv? ,eqv?)
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
	    baseenv))

(let () ;; new scope to suppress toplevel expr result printing
  (vau-eval `($load! "prelude.vau") baseenv)
  (void))
