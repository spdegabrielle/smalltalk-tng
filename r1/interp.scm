(require 'srfi-1)

(require 'util)
(require 'parsetng)

;; This is just documentation. It's not used anywhere.
(define %location-protocol
  '(namespace-traits
    (loc at: sym)
    (loc at: sym put: val)

    continuation-traits
    (loc return: val)
    (loc raise: exn)

    required-continuation-traits
    (loc continuation-cell)

    concurrency-traits
    (loc parent)
    (loc children-cell)))

(define runnable-suspensions '())
;;(define runnable-suspension-counter 0)
(define current-location #f)

(define (metalevel-work-available?)
  (not (null? runnable-suspensions)))

(define (metalevel-resume-thread! suspension value)
  ;;(set! runnable-suspension-counter (+ runnable-suspension-counter 1))
  (push! runnable-suspensions (lambda () (suspension value))))

(define (metalevel-location-alive? location)
  (or (eq? *nil* location)
      (*false*? (get-slot location 'dead))))

(define (metalevel-unsuspend location thunk)
  (if (metalevel-location-alive? location)
      (begin
	(set! current-location location)
	(thunk))
      (metalevel-schedule!!)))

(define (metalevel-spawn location thunk)
  ;;(set! runnable-suspension-counter (+ runnable-suspension-counter 1))
  (push! runnable-suspensions (lambda () (metalevel-unsuspend location thunk))))

(define (metalevel-suspend-thread receiver)
  (call-with-current-continuation
   (lambda (k)
     (let ((location current-location))
       (receiver (lambda (v) (metalevel-unsuspend location (lambda () (k v)))))
       (metalevel-schedule!!)))))

(define metalevel-root-continuation #f)
(define (metalevel-schedule!!)
  (metalevel-root-continuation 'throw))

(define -metalevel-running #t)
(define (metalevel-stop!)
  (set! -metalevel-running #f))
(define (metalevel-stopped?)
  (not -metalevel-running))

(define (metalevel-run-runnable-suspensions next-event-time)
  (call-with-current-continuation
   (lambda (restart-mainloop)
     (set! metalevel-root-continuation restart-mainloop)))
  ;;(write (list "Suspensions: "runnable-suspension-counter))(newline)
  (do ()
      ((or (and next-event-time (>= (get-time-of-day) next-event-time))
	   (not -metalevel-running)
	   (not (metalevel-work-available?))))
    (let ((suspension (car runnable-suspensions)))
      ;;(set! runnable-suspension-counter (- runnable-suspension-counter 1))
      (set! runnable-suspensions (cdr runnable-suspensions))
      (suspension)))
  (let ((now (get-time-of-day)))
    (if (and next-event-time
	     (< now next-event-time))
	(sdl-delay (truncate (* (- next-event-time now) 1000.0))))))

(define (metalevel-suspend-on-cell c)
  (metalevel-suspend-thread
   (lambda (suspension)
     (set-slot! c 'queue (cons suspension (get-slot c 'queue))))))

(define (metalevel-peek-cell-value c)
  (let ((val (get-slot c '_pvt_value)))
    (if (eq? val *no-role*)
	(let ((newval (metalevel-suspend-on-cell c)))
	  (set-slot! c '_pvt_value newval)
	  newval)
	val)))

(define (metalevel-extract-cell-value c)
  (let ((val (get-slot c '_pvt_value)))
    (if (eq? val *no-role*)
	(metalevel-suspend-on-cell c)
	(begin
	  (set-slot! c '_pvt_value *no-role*)
	  val))))

(define (metalevel-inject-cell-value c v)
  (if (eq? (get-slot c '_pvt_value) *no-role*)
      (let ((q (get-slot c 'queue)))
	(if (pair? q)
	    (let ((suspension (car q))
		  (remainder (cdr q)))
	      (set-slot! c 'queue remainder)
	      (metalevel-resume-thread! suspension v))
	    (set-slot! c '_pvt_value v))
	*nil*)
      (metalevel-raise-exception (list 'cellOverflow c))))

(define (extend-env name val env)
  (cons (cons name val) env))

(define metalevel-raise-exception error)

(define metalevel-interpret
  (let ()
    (define (do-local resend env lit instr)
      (cdr (assq (vector-ref instr 1) env)))

    (define (do-global resend env lit instr)
      (let ((dict (metalevel-peek-cell-value *globals*)))
	(send/previous-method #f (vector-ref instr 1) (vector dict))))

    (define (eval-in-par thunk)
      (let* ((cell (clone-object *cell*))
	     (loc (clone-object *location*)))
	(set-slot! loc 'continuation cell)
	(set-slot! loc 'parent current-location)
	(set-slot! current-location 'children (cons loc (get-slot current-location 'children)))
	(metalevel-spawn loc (lambda () (metalevel-inject-cell-value cell (thunk))))
	cell))

    (define (eval-tuple resend env lit instrs)
      (let ((num-instrs (vector-length instrs)))
	(if (= num-instrs 1)
	    (vector (vm resend env lit (vector-ref instrs 0)))
	    (let ((vals (make-vector num-instrs))
		  (flags (make-vector num-instrs)))
	      (do ((index 0 (+ index 1)))
		  ((= index num-instrs))
		(let* ((instr (vector-ref instrs index))
		       (kind (vector-ref instr 0))
		       (flag (or (eq? kind 'local)
				 (eq? kind 'literal))))
		  (vector-set! flags index flag)
		  (vector-set! vals index
			       (if flag
				   (vm resend env lit instr)
				   (eval-in-par (lambda () (vm resend env lit instr)))))))
	      (do ((index 0 (+ index 1)))
		  ((= index num-instrs))
		(if (not (vector-ref flags index))
		    (vector-set! vals index
				 (metalevel-peek-cell-value (vector-ref vals index)))))
	      vals))))

    (define (do-send resend env lit instr)
      (let ((selector (vector-ref instr 1))
	    (vals (eval-tuple resend env lit (vector-ref instr 2))))
	(debug 2 --> 0 "Send "selector" "vals)
	(let ((result (send/previous-method #f selector vals)))
	  (debug 2 --> 0 "Rslt "selector" "vals" ==> "result)
	  result)))

    (define (do-closure resend env lit instr)
      (let* ((block (clone-object (vector-ref lit (vector-ref instr 1)))))
	(set-slot! block 'environment env)
	block))

    (define (do-begin resend env lit instr)
      (eval-statements resend env lit (vector-ref instr 1)))

    (define (do-scope resend env lit instr)
      (let* ((name (vector-ref instr 1))
	     (cell (eval-in-par (lambda ()
				  (let ((newenv (extend-env name current-location env)))
				    (eval-statements resend newenv lit (vector-ref instr 2)))))))
	(metalevel-peek-cell-value cell)))

    (define (do-literal resend env lit instr)
      (vector-ref lit (vector-ref instr 1)))

    (define (do-update resend env lit instr)
      (let* ((o (clone-object (vm resend env lit (vector-ref instr 1))))
	     (updates (vector-ref instr 2))
	     (n (vector-length updates)))
	(do ((i 0 (+ i 1)))
	    ((= i n))
	  (let ((update (vector-ref updates i)))
	    (let ((delegating (eq? (vector-ref update 0) *true*))
		  (name (vector-ref update 1))
		  (update-instr (vector-ref update 2)))
	      (let ((val (vm resend env lit update-instr)))
		(if (has-slot? o name)
		    (set-slot! o name val)
		    (add-slot! o name val delegating 'immutable))))))
	o))

    (define (do-tuple resend env lit instr)
      (eval-tuple resend env lit (vector-ref instr 1)))

    (define (do-resend resend env lit instr)
      (resend))

    (define (do-method resend env lit instr)
      (let ((selector (vector-ref instr 1))
	    (formals (vector-ref instr 2))
	    (specializer-instrs (vector-ref instr 3))
	    (body-object (vector-ref instr 4))
	    (method-litvec (vector-ref instr 5)))
	(let* ((specializers (map (lambda (specializer-instr)
				    (vm resend env lit specializer-instr))
				  (vector->list specializer-instrs)))
	       (method (define-method! selector formals specializers body-object)))
	  (set-slot! method 'literals method-litvec)
	  *nil*)))

    (define (eval-statement resend env lit statement k)
      (if (eq? (vector-ref statement 0) 'bind)
	  (let* ((name (vector-ref statement 1))
		 (instr (vector-ref statement 2))
		 (newenv (extend-env name *nil* env))
		 (value (vm resend newenv lit instr)))
	    (set-cdr! (car newenv) value)
	    (k newenv value))
	  (k env (vm resend env lit statement))))

    (define (eval-statements resend env lit statements)
      (let ((n (vector-length statements)))
	(let loop ((env env)
		   (i 0)
		   (acc *nil*))
	  (if (= i n)
	      acc
	      (eval-statement resend env lit (vector-ref statements i)
			      (lambda (newenv value)
				(loop newenv (+ i 1) value)))))))

    (define optable (make-hash-table eq?))

    (define (vm resend env lit instr)
      (debug 1 --> 0 "Eval "instr)
      (debug 2 --> 0 "Env= "env)
      (let ((result ((hash-table-ref optable (vector-ref instr 0)
				     (lambda _ (error "Unknown instruction" instr)))
		     resend env lit instr)))
	(debug 2 --> 0 "Done "instr" ==> "result)
	result))

    (hash-table-set! optable 'local do-local)
    (hash-table-set! optable 'global do-global)
    (hash-table-set! optable 'send do-send)
    (hash-table-set! optable 'closure do-closure)
    (hash-table-set! optable 'begin do-begin)
    (hash-table-set! optable 'scope do-scope)
    (hash-table-set! optable 'literal do-literal)
    (hash-table-set! optable 'update do-update)
    (hash-table-set! optable 'tuple do-tuple)
    (hash-table-set! optable 'resend do-resend)
    (hash-table-set! optable 'method do-method)

    vm))

(define (metalevel-eval-method code method argv)
  (let* ((litvec (get-slot method 'literals))
	 (prologue (car code))
	 (instruction (cdr code))
	 (need-block-environment? (eq? *true* (vector-ref prologue 1))))
    (bump-invocation-count! prologue method)
    (metalevel-interpret (if need-block-environment?
			     #f
			     (lambda ()
			       (send/previous-method method (get-slot method 'selector) argv)))
			 (fold extend-env
			       (if need-block-environment?
				   (get-slot (vector-ref argv 0) 'environment)
				   '())
			       (get-slot method 'arguments)
			       (vector->list argv))
			 litvec
			 instruction)))

(define (metalevel-eval ast)
  (let-values (((instr litvec) (compile-ThiNG ast)))
    (metalevel-interpret #f '() litvec instr)))

(define (ThiNG-load-file filename)
  (let-values (((success ast) (call-with-input-file filename
				(lambda (port)
				  (parse-ThiNG filename
					       ThiNG-parser
					       (lambda () (read-char port)))))))
    (if success
	(cons *true* (metalevel-eval `(scope ,*nil* ,ast)))
	(cons *false* ast))))
