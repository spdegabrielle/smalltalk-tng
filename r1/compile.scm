;; Compile AST to a set of prototype methods and blocks.

(define-record-type compilation-state
  (make-compilation-state* next-literal rev-literals)
  compilation-state?
  (next-literal compilation-state-next-literal)
  (rev-literals compilation-state-rev-literals))

(define (make-compilation-state)
  (make-compilation-state* 0 '()))

(define (push-literal state val)
  (let ((i (compilation-state-next-literal state)))
    (values i
	    (make-compilation-state* (+ i 1)
				     (cons val (compilation-state-rev-literals state))))))

(define (finish-compilation-state state)
  (list->vector (reverse (compilation-state-rev-literals state))))

(define *all-method-code-prologues* '())
(define *invocation-count-decay-half-life* 15) ;; seconds
(define *invocation-count-update-interval* 4) ;; seconds
(define *recompilation-count-limit* 1000)

(define (instruction->code instr is-closure)
  (let ((prologue (vector 0 (if is-closure *true* *false*))))
    (let ((locative (make-weak-locative prologue 0)))
      (push! *all-method-code-prologues* locative))
    (cons prologue instr)))

(define (invocation-count-decay-constant)
  (exp (/ (log 2)
	  (/ *invocation-count-decay-half-life* *invocation-count-update-interval*))))

(define (decay-invocation-counts!)
  (debug 0 "Decaying invocation counts...")
  (let ((decay-constant (invocation-count-decay-constant)))
    (set! *all-method-code-prologues*
	  (filter! (lambda (locative)
		     (let ((prologue (locative->object locative)))
		       (if prologue
			   (vector-set! prologue 0 (/ (vector-ref prologue 0) decay-constant)))
		       prologue))
		   *all-method-code-prologues*))))

(define (bump-invocation-count! prologue method)
  (let ((invocation-count (+ (vector-ref prologue 0) 1)))
    (vector-set! prologue 0 invocation-count)
    (if (>= invocation-count *recompilation-count-limit*)
	(begin
	  (vector-set! prologue 0 0)
	  (recompile-method! method)))))

(define compile-ThiNG
  (let ()
    (define (do-ref cenv state name)
      (let* ((name (string->symbol name)))
	(values (cond ((memq name cenv) `#(local ,name))
		      (else `#(global ,name)))
		state)))

    (define (compile-tuple cenv state exprs)
      (let loop ((exprs exprs)
		 (state state)
		 (acc '()))
	(if (null? exprs)
	    (values (list->vector (reverse acc)) state)
	    (let*-values (((instr state) (compile cenv state (car exprs))))
	      (loop (cdr exprs)
		    state
		    (cons instr acc))))))

    (define (do-send cenv state selector exprs)
      (let-values (((selector) (string->symbol selector))
		   ((instrs state) (compile-tuple cenv state exprs)))
	(values `#(send ,selector ,instrs)
		state)))

    (define (do-block cenv state binders statements)
      (let* ((block (clone-object *block*))
	     (num-formals (length binders))
	     (formals (map string->symbol binders))
	     (formal-cenv (append (cons '_ formals) cenv))
	     (selector (if (zero? num-formals)
			   'do
			   (string->symbol
			    (string-concatenate (cons "applyWith:"
						      (make-list (- num-formals 1) "with:")))))))
	(let*-values (((instr block-state)
		       (compile formal-cenv (make-compilation-state)
				`(scope ,*nil* ,statements)))
		      ((litvec) (finish-compilation-state block-state))
		      ((method) (define-method! selector (cons '_ formals) (list block)
				  (instruction->code instr #t)))
		      ((block-index state) (push-literal state block)))
	  (set-slot! method 'literals litvec)
	  (values `#(closure ,block-index)
		  state))))

    (define (do-scope cenv state name statements)
      (if (eq? *nil* name)
	  (let-values (((instrs state) (compile-statements cenv state statements)))
	    (values `#(begin ,instrs)
		    state))
	  (let*-values (((name) (string->symbol name))
			((instrs state) (compile-statements (cons name cenv) state statements)))
	    (values `#(scope ,name ,instrs)
		    state))))

    (define (do-literal cenv state val)
      (let-values (((index state) (push-literal state val)))
	(values `#(literal ,index)
		state)))

    (define (do-update cenv state template-expr updates)
      (let*-values (((template-instr state) (compile cenv state template-expr))
		    ((updates state)
		     (let loop ((updates updates)
				(state state)
				(acc '()))
		       (if (null? updates)
			   (values (list->vector (reverse acc)) state)
			   (let*-values (((update) (car updates))
					 ((update-instr state)
					  (compile cenv state (caddr update))))
			     (loop (cdr updates)
				   state
				   (cons (vector (car update)
						 (string->symbol (cadr update))
						 update-instr)
					 acc)))))))
	(values `#(update ,template-instr ,updates)
		state)))

    (define (do-tuple cenv state exprs)
      (let-values (((instrs state) (compile-tuple cenv state exprs)))
	(values `#(tuple ,instrs)
		state)))

    (define (do-resend cenv state)
      (values `#(resend)
	      state))

    (define (do-method cenv state pattern statements)
      (let* ((selector (string->symbol (cadr pattern)))
	     (params (caddr pattern))
	     (formals (map (lambda (entry) (string->symbol (or (non-*false*? (car entry))
							       "_")))
			   params)))
	(let*-values (((specializer-instrs state)
		       (compile-tuple cenv state (map (lambda (entry)
							(let ((exp (cadr entry)))
							  (if (non-*false*? exp)
							      exp
							      `(ref "NoRole"))))
						      params)))
		      ((body-instr method-state)
		       (compile formals (make-compilation-state) `(scope ,*nil* ,statements)))
		      ((method-litvec) (finish-compilation-state method-state)))
	  (values `#(method ,selector ,formals ,specializer-instrs
			    ,(instruction->code body-instr #f)
			    ,method-litvec)
		  state))))

    (define (compile-statement cenv state statement)
      (if (and (pair? statement)
	       (eq? (car statement) 'let))
	  (let* ((name (string->symbol (cadr statement)))
		 (expr (caddr statement))
		 (newenv (cons name cenv)))
	    (let-values (((instr state) (compile newenv state expr)))
	      (values `#(bind ,name ,instr)
		      newenv
		      state)))
	  (let-values (((instr state) (compile cenv state statement)))
	    (values instr cenv state))))

    (define (compile-statements cenv state statements)
      (let loop ((cenv cenv)
		 (state state)
		 (statements statements)
		 (acc '()))
	(if (null? statements)
	    (values (list->vector (reverse acc))
		    state)
	    (let-values (((instr cenv state) (compile-statement cenv state (car statements))))
	      (loop cenv
		    state
		    (cdr statements)
		    (cons instr acc))))))

    (define (compile cenv state ast)
      (debug 1 "compile "ast" "cenv)
      (cond
       ((pair? ast)
	(apply (cond
		((assq (car ast) `((ref ,do-ref)
				   (send ,do-send)
				   (block ,do-block)
				   (scope ,do-scope)
				   (string ,do-literal)
				   (symbol ,do-literal)
				   (number ,do-literal)
				   (update ,do-update)
				   (tuple ,do-tuple)
				   (resend ,do-resend)
				   (method ,do-method)
				   )) => cadr)
		(else (error "Unknown ast kind" ast)))
	       cenv state
	       (cdr ast)))
       (else (error "Non-pair ast" ast))))

    (lambda (ast)
      (let-values (((instr state) (compile '() (make-compilation-state) ast)))
	(values instr
		(finish-compilation-state state))))))

(define (instruction-vector-size seed instr-vec)
  (vector-fold (lambda (instr acc) (+ (instruction-size instr) acc)) seed instr-vec))

(define (instruction-size instr)
  (case (vector-ref instr 0)
    ((local global closure literal resend) 1)
    ((send) (instruction-vector-size 1 (vector-ref instr 2)))
    ((begin) (instruction-vector-size 0 (vector-ref instr 1)))
    ((scope) (instruction-vector-size 0 (vector-ref instr 2)))
    ((update) (instruction-vector-size 1 (vector-ref instr 2)))
    ((tuple) (instruction-vector-size 1 (vector-ref instr 1)))
    ((method) 1) ;; not quite correct, but mneh. until the macro is expanded properly, will do.
    (else (error "Illegal instruction in instruction-size" instr))))

(define (recompile-method! method)
  (let ((instr (cdr (get-slot method 'code))))
    (pretty-print `(recompile-method!
		    (size ,(instruction-size instr))
		    (instr ,instr)))))
