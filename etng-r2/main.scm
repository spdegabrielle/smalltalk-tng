(require srfi/1) ;; lists
(require srfi/4) ;; homogeneous-numeric-vectors, u8vector
(require srfi/8) ;; receive
(require srfi/9) ;; records
(require srfi/13) ;; strings
(require scheme/pretty)

(print-struct #t)
(define previous-inspector (current-inspector))
(current-inspector (make-inspector))

(define-record-type tng-qname
  (make-qname uri localname)
  qname?
  (uri qname-uri)
  (localname qname-localname))

(current-inspector previous-inspector)

(require "../../ometa-scheme/ometa.scm")
(ometa-library-path "../../ometa-scheme")

(define etng-naked-id-terminators (string->list "`.()[]{}:;,'\""))

(define (char-etng-id-alpha? ch)
  (or (char-alphabetic? ch)
      (eqv? ch #\_)))

(define (char-etng-id-punct? ch)
  (not (or (char-alphabetic? ch)
	   (char-whitespace? ch)
	   (char-numeric? ch)
	   (memv ch etng-naked-id-terminators))))

(define (eol-char? c)
  (or (eqv? c #\return)
      (eqv? c #\newline)))

(define (qname-or-symbol? x)
  (or (qname? x)
      (symbol? x)))

(define EMPTY-SYMBOL (string->symbol ""))
(define QUOTE-QNAME (make-qname EMPTY-SYMBOL 'quote))
(define UNQUOTE-QNAME (make-qname EMPTY-SYMBOL 'unquote))
(define SEMI (string->symbol ";"))
(define COMMA (string->symbol ","))
(define ARROW '->)
(define DISCARD '_)
(define PIPE (string->symbol "|"))

(define (list-interleave x xs)
  (cond
   ((null? xs) '())
   ((null? (cdr xs)) xs)
   (else (cons (car xs) (cons x (list-interleave x (cdr xs)))))))

(define (invert-sign x) (- x))

(define (etng-sexp-special-match? sexps qname)
  (and (pair? sexps)
       (let ((tok (car sexps)))
	 (equal? tok qname))))

(define (special-segment-head? token)
  (or (equal? token QUOTE-QNAME)
      (equal? token UNQUOTE-QNAME)
      (memq token '(namespace do let %assemble))))

(define (->string x)
  (cond
   ((string? x) x)
   ((symbol? x) (symbol->string x))
   ((qname? x) (string-append (->string (qname-uri x))
			      ":"
			      (->string (qname-localname x))))
   (else (let ((s (open-output-string)))
	   (write x s)
	   (get-output-string s)))))

(define read-etng* (load-ometa "etng-reader.g"))
(define parse-etng* (load-ometa "etng-parser.g"))

(define (read-etng input ks kf)
  (read-etng* 'sexp input ks kf))

(define (read-etng-toplevel input ks kf)
  (read-etng* 'sexp-toplevel input ks kf))

(define pass-common (opt (parse-ometa-file "etng-pass-common.g")))

(define (load-pass grammar-filename)
  (let ((g (merge-ometa pass-common (parse-ometa-file grammar-filename))))
    (lambda (input)
      (simple-ometa-driver g
			   'pass
			   (->input-stream (list input))
			   (lambda (result next err) result)
			   (lambda (err)
			     (pretty-print `(,grammar-filename ,err))(newline)
			     #f)))))

(define null-pass (load-pass "etng-null-pass.g"))

(define convert-constant-methods-pass (load-pass "etng-convert-constant-methods-pass.g"))

(define (convert-constant-methods ast-prefix methods)
  (let loop ((methods methods)
	     (reversed-temporaries '())
	     (reversed-initializers '())
	     (transformed-methods '()))
    (if (null? methods)
	(let ((new-methods (reverse transformed-methods))
	      (temporaries (reverse reversed-temporaries))
	      (initializers (reverse reversed-initializers)))
	  (cond
	   ((null? temporaries)
	    `(,@ast-prefix ,@new-methods))
	   ((null? (cdr temporaries))
	    `(send (function (method ((bind ,(car temporaries)))
				     (,@ast-prefix ,@new-methods)))
		   ,(car initializers)))
	   (else
	    `(send (function (method ((tuple ,@(map (lambda (temp) `(bind ,temp)) temporaries)))
				     (,@ast-prefix ,@new-methods)))
		   (tuple ,@initializers)))))
	(let ((method (car methods)))
	  (if (eq? (car method) 'constant-method)
	      (let ((temp (gensym)))
		(loop (cdr methods)
		      (cons temp reversed-temporaries)
		      (cons (caddr method) reversed-initializers)
		      (cons `(method ,(cadr method) (ref ,temp)) transformed-methods)))
	      (loop (cdr methods)
		    reversed-temporaries
		    reversed-initializers
		    (cons method transformed-methods)))))))

(define (alpha-convert-expr exp conversions)
  (define (convert-method method)
    `(,(car method)
      ,(map (lambda (p) (alpha-convert-pattern p conversions)) (cadr method))
      ,(alpha-convert-expr (caddr method) conversions)))
  (case (car exp)
    ((ref) `(ref ,(cond ((assq (cadr exp) conversions) => cadr) (else (cadr exp)))))
    ((lit) exp)
    ((object) `(,(car exp) ,(cadr exp) ,@(map convert-method (cddr exp))))
    ((function) `(,(car exp) ,@(map convert-method (cdr exp))))
    ((tuple) `(tuple ,@(map (lambda (x) (alpha-convert-expr x conversions)) (cdr exp))))
    ((send) `(send ,@(map (lambda (x) (alpha-convert-expr x conversions)) (cdr exp))))))

(define (alpha-convert-pattern pat conversions)
  (case (car exp)
    ((discard) exp)
    ((bind) `(bind ,(cond ((assq (cadr exp) conversions) => cadr) (else (cadr exp)))))
    ((lit) exp)
    ((tuple) `(tuple ,@(map (lambda (p) (alpha-convert-pattern p conversions)) (cdr exp))))))

(define (names-in-pattern p)
  (case (car p)
    ((discard) '())
    ((bind) (list (cadr p)))
    ((lit) '())
    ((tuple) (append-map names-in-pattern (cdr p)))))

(define (etng-sexp->string-tree e)
  (cond
   ((pair? e) (cond
	       ((and (eq? (car e) 'paren)
		     (pair? (cdr e))
		     (equal? (cadr e) QUOTE-QNAME))
		`("." ,(etng-sexp->string-tree (caddr e))))
	       (else
		((case (car e)
		   ((paren) (lambda (es) `("(" ,es ")")))
		   ((brack) (lambda (es) `("[" ,es "]")))
		   ((brace) (lambda (es) `("{" ,es "}")))
		   (else (error 'illegal-sexp e)))
		 (list-interleave " " (map etng-sexp->string-tree (cdr e)))))))
   ((string? e) e)
   (else (->string e))))

(define (cons-tree-for-each f l)
  (let walk ((l l))
    (if (pair? l)
	(begin (walk (car l)) (walk (cdr l)))
	(f l))))

(define (pp clue x . maybe-transformer)
  (pretty-print (list clue 
		      (if (null? maybe-transformer)
			  x
			  ((car maybe-transformer) x))))
  (newline)
  x)

(define (!pp clue x . maybe-transformer)
  x)

(define (dump-string-tree t)
  (cons-tree-for-each (lambda (x) (or (null? x) (display x))) t)
  (newline))

(define (mark-position pos-path t)
  (if (null? pos-path)
      t
      (call-with-values (lambda () (split-at t (car pos-path)))
	(lambda (left right)
	  (if (null? (cdr pos-path))
	      (append left (list "<@@@@@>") right)
	      (append left (list (mark-position (cdr pos-path) (car right))) (cdr right)))))))

(define (display-parse-error clue err . maybe-ast)
  (display clue)
  (newline)
  (when (not (null? maybe-ast))
    (dump-string-tree (etng-sexp->string-tree (car (mark-position (car err)
								  (list (car maybe-ast)))))))
  (display (format-ometa-error err))
  (newline))

(define (parse-print-and-eval sexp evaluator)
  ;; (pp 'raw-sexp sexp) (newline)
  (dump-string-tree (etng-sexp->string-tree sexp))
  (parse-etng* 'toplevel (list sexp)
	       (lambda (ast dummy-next err)
		 (if (null? (input-stream->list dummy-next))
		     (evaluator ast)
		     (display-parse-error "Partial parse." err sexp)))
	       (lambda (err)
		 (display-parse-error "Unsuccessful parse." err sexp))))

(load "compile-to-scheme.scm")

(define (rude-evaluator input)
  (let* ((ast (!pp 'ast input))
	 (ast (!pp 'convert-constant-methods-pass (convert-constant-methods-pass ast)))
	 (scheme-ast (!pp 'compile-to-scheme (compile-to-scheme ast)))
	 (thunk (!pp 'compile-scheme (eval `(lambda () ,scheme-ast))))
	 )
    (write (thunk))
    (newline)))

(define (etng-parse-file* filename evaluator)
  (call-with-input-file filename
    (lambda (handle)
      (let loop ((input (->input-stream handle)))
	(read-etng-toplevel
	 input
	 (lambda (sexp0 next err)
	   (if (eq? sexp0 'eof)
	       'eof-reached
	       (let ((sexp (cons 'paren sexp0)))
		 (parse-print-and-eval sexp evaluator)
		 (when (and next (not (eq? next input)))
		   (loop next)))))
	 (lambda (error-description)
	   (display-parse-error "Reader failure." error-description)))))))

(define (etng-parse-file filename)
  (etng-parse-file* filename rude-evaluator))

(define (etng-repl* evaluator)
  (let loop ((input (current-input-stream)))
    (display ">>ETNG>> ")
    (flush-output)
    (read-etng-toplevel
     input
     (lambda (sexp0 next err)
       (if (eq? sexp0 'eof)
	   'eof-reached
	   (let ((sexp (cons 'paren sexp0)))
	     (parse-print-and-eval sexp evaluator)
	     (when (and next (not (eq? next input)))
	       (loop next)))))
     (lambda (error-description)
       (display-parse-error "Reader failure." error-description)
       (loop (current-input-stream))))))

(define (etng-repl)
  (etng-repl* rude-evaluator))

(etng-parse-file "boot.tng")
(etng-repl)
