(require 'srfi-1)
(require 'srfi-13)
(load "json-scheme/portable-packrat.scm")
(load "../lib/pregexp-20050502/pregexp.scm")

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
		      (define-packrat-cached atom (packrat-regex 'atom "[A-Z]"midsym"*"))
		      (define-packrat-cached var (packrat-regex 'var "[a-z]"midsym"*"))
		      (define-packrat-cached infixop-raw (packrat-regex 'infixop p midsym"*"))
		      (define-packrat-cached integer (packrat-regex 'integer "[0-9]+"))
		      (define (rewrite-infix parts)
			(let loop ((left (second parts))
				   (parts parts))
			  (let* ((at-end (car parts)) (parts (cdr parts))
						      (parts (cdr parts))
				 (op (car parts))     (parts (cdr parts))
				 (rest (car parts)))
			  (if at-end
			      `(adj ,op (adj (tuple ,left ,rest) (tuple)))
			      (loop `(adj ,op (adj (tuple ,left ,(second rest)) (tuple)))
				    rest)))))
		      (values tuple1 toplevel))
		    (toplevel ((d <- tuple0 white '#\; '#\;) d))
		    (datum ((s <- tuple0) s))
		    (tuple0 ((s <- tuple1) s)
			    (() '(tuple)))
		    (tuple1 ((s <- tuple1*) (if (= (length s) 2) (cadr s) s)))
		    (tuple1* ((d <- dict white '#\, s <- tuple1*) `(tuple ,d ,@(cdr s)))
			     ((d <- dict) `(tuple ,d)))
		    (dict ((e <- entry white d <- dict) `(dict ,e ,@(cdr d)))
			  ((e <- entry) `(dict ,e))
			  ((v <- funcall) v))
		    (entry ((k <- simple colon v <- funcall) (list k v)))
		    (semi ((white '#\; (! '#\;)) 'semi))
		    (colon ((white '#\:) 'colon))
		    (funcall ((parts <- funcall*) (rewrite-infix parts))
			     ((a <- adj) a))
		    (funcall* ((a <- adj o <- infixop b <- funcall*) (list #f a o b))
			      ((a <- adj o <- infixop b <- adj) (list #t a o b)))
		    (infixop ((white r <- infixop-raw) `(atom ,(string->symbol r))))
		    (adj ((a <- adj*) (if (equal? (caddr a) '(tuple)) (cadr a) a)))
		    (adj* ((v <- simple white vs <- adj*) `(adj ,v ,vs))
			  ((v <- simple semi vs <- simple) `(adj ,v ,vs))
			  ((v <- simple (! colon)) `(adj ,v (tuple))))
		    (simple ((white d1 <- simple1) d1))
		    (simple1 (('#\( o <- infixop white '#\)) o)
			     (('#\( d <- datum white '#\)) d)
			     (('#\{ d <- datum white '#\}) `(quote ,d))
			     (('#\[ d <- datum white '#\]) `(meta ,d))
			     ((l <- literal) `(lit ,l))
			     ((a <- atom) `(atom ,(string->symbol a)))
			     ((a <- var) `(var ,(string->symbol a)))
			     (('#\_) `(discard)))
		    (literal ((i <- integer) (string->number i))))))

(define read-ThiNG
  (lambda ()
    (parse-result->value "While parsing ThiNG"
			 (parse-ThiNG-toplevel (port-results "stdin" (current-input-port))))))

(define string->ThiNG
  (lambda (s)
    (parse-result->value "While parsing ThiNG"
			 (parse-ThiNG (string-results "<string>" s)))))

; (define (cst->v cst)
;   (if (pair? cst)
;       (cond
;        ((eq? (car cst) 'adj)
; 	(cons (cst->v (cadr cst))
; 	      (cst->v (caddr cst))))
;        ((and (eq? (car cst) 'tuple)
; 	     (null? (cdr cst)))
; 	'())
;        ((eq? (car cst) 'quote)
; 	(list 'quote (cst->v (cadr cst))))
;        (else
; 	(list->vector (map cst->v cst))))
;       cst))

(define (repl-ThiNG)
  (display ">>>ThiNG>>> ")
  (let ((x (read-ThiNG)))
    (newline)
    ;;(pretty-print (cst->v x))
    (pretty-print x)
    (newline)
    (if (not (equal? x '(atom Quit)))
	(repl-ThiNG))))

"
define map {
  (_ f Nil)           : Nil
  (_ f (Hd: h Tl: t)) : (Hd: f h Tl: map f t)
};;

define fold-left {
  (_ kons knil Nil)           : knil
  (_ kons knil (Hd: h Tl: t)) : fold-left kons (kons h knil) t
};;

map {x: x + 1} [1, 2, 3];;

map {x: x + 1} (list 1 2 3);;
"
