(require (lib "match.ss")
	 (lib "etc.ss")
	 (lib "errortrace.ss" "errortrace")
	 (lib "9.ss" "srfi"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print-struct #t)
(define previous-inspector (current-inspector))
(current-inspector (make-inspector))

(define-record-type <monad-class>
  (make-monad-class name binder returner failer)
  monad-class?
  (name monad-class-name)
  (binder monad-class-binder)
  (returner monad-class-returner)
  (failer monad-class-failer))

(define-record-type <monad>
  (make-monad kind value)
  monad?
  (kind monad-kind)
  (value monad-value))

(current-inspector previous-inspector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (determined? m)
  (monad-class? (monad-kind m)))

(define (monadize m)
  (cond
   ((pair? m) (make-monad *list* m))
   ((null? m) (make-monad *list* m))
   ((monad? m) (if (eq? (monad-kind m) '_delayed)
		   ((monad-value m))
		   m))
   (else (error "not a monad" m))))

(define-syntax delay-monad
  (syntax-rules ()
    ((_ m) (delay-monad* (lambda () (undelay-monad m))))))

(define (delay-monad* m)
  (make-monad '_delayed m))

(define (undelay-monad m)
  (if (and (monad? m) (eq? (monad-kind m) '_delayed))
      ((monad-value m))
      m))

(define (>>= ma a->mb)
  (let ((ma (monadize ma)))
    (if (determined? ma)
	((monad-class-binder (monad-kind ma)) ma a->mb)
	(make-monad '_bind (list ma a->mb)))))

(define (return a)
  (make-monad '_return a))

(define (fail s)
  (make-monad '_fail s))

(define-syntax mlet*
  (syntax-rules ()
    ((_ () mexpN) mexpN)
    ((_ ((var mexp) rest ...) mexpN)
     (>>= mexp (lambda (var) (mlet* (rest ...) mexpN))))))

(define (wrong-mclass mclass m)
  (error "wrong monad-class" `((mclass ,mclass) (m ,m))))

(define (determine mclass m)
  (let continue ((m m))
    (let* ((m (monadize m))
	   (kind (monad-kind m)))
      (if (determined? m)
	  (if (eq? kind mclass)
	      m
	      (wrong-mclass mclass m))
	  (continue
	   (case kind
	     ((_bind) ((monad-class-binder mclass)
		       (continue (car (monad-value m)))
		       (cadr (monad-value m))))
	     ((_return) ((monad-class-returner mclass) (monad-value m)))
	     ((_fail) ((monad-class-failer mclass) (monad-value m)))
	     (else
	      (error "invalid monad-kind" m))))))))

(define (monad-arg mclass)
  (lambda (m) (monad-value (determine mclass m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *list* (make-monad-class 'list
				 (lambda (L f) (append-map (compose run-list f) (run-list L)))
				 (lambda (x) (list x))
				 (lambda (s) '())))

(define run-list (monad-arg *list*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *io* (make-monad-class 'io
			       (lambda (io1 f) (delay-monad (f (run-io io1))))
			       (lambda (v) (make-monad *io* (lambda () v)))
			       error))

(define io-action (monad-arg *io*))

(define (run-io m)
  ((io-action m)))

(define (mdisplay x)
  (make-monad *io* (lambda () (display x) 'done)))

(define mread
  (make-monad *io* (lambda () (read))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *state* (make-monad-class 'state
				  (lambda (st1 f)
				    (make-monad *state*
						(lambda (s0)
						  (let* ((inp (run-st st1 s0))
							 (v (car inp))
							 (s1 (cdr inp)))
						    (run-st (f v) s1)))))
				  (lambda (a)
				    (make-monad *state*
						(lambda (s0) (cons a s0))))
				  error))

(define state-xformer (monad-arg *state*))

(define (run-st m initial)
  ((state-xformer m) initial))

(define sget
  (make-monad *state* (lambda (s0) (cons s0 s0))))

(define (sput a)
  (make-monad *state* (lambda (s0) (cons 'unit a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (lib "1.ss" "srfi"))

(define (mixed-monad-demo)
  (run-io (mlet* ((_ (mdisplay "Enter a number\n"))
		  (n mread)
		  (all-n (return (iota n)))
		  (_ (mdisplay "Numbers: "))
		  (_ (mdisplay all-n))
		  (_ (mdisplay "\n")))
		 (return 'nothing))))

(define oleg-example-mixed-monad
  (mlet* ((_ (mdisplay "Enter a number: "))
	  (n mread)
	  (all-n (return (iota n)))
	  (evens (return (run-list (mlet* ((i all-n))
				     (if (even? i)
					 (return i)
					 (fail "odd"))))))
	  (_ (mdisplay "Computed "))
	  (_ (mdisplay (length evens)))
	  (_ (mdisplay " evens\n")))
    (return evens)))
