(require (lib "match.ss")
	 (lib "9.ss" "srfi"))

(define-record-type <monad>
  (make-monad kind value)
  monad?
  (kind monad-kind)
  (value monad-value))

(define (>>= ma a->mb)
  (make-monad '_bind (list ma a->mb)))

(define (return a)
  (make-monad '_return a))

(define-syntax mlet*
  (syntax-rules ()
    ((_ () mexpN) mexpN)
    ((_ ((var mexp) rest ...) mexpN)
     (>>= mexp (lambda (var) (mlet* (rest ...) mexpN))))))

(define (determine kind binder returner m)
  (let continue ((m m))
    (if (not (monad? m))
	(error "not a monad" m)
	(case (monad-kind m)
	  ((_bind)
	   (continue (binder (continue (car (monad-value m)))
			     (cadr (monad-value m)))))
	  ((_return)
	   (returner (monad-value m)))
	  (else
	   m)))))

(define (monad-arg k)
  (lambda (m) (if (eq? (monad-kind m) k)
		  (monad-value m)
		  (error "wrong type of monad" `((kind ,k) (m ,m))))))

(define io-action (monad-arg 'io))

(define (mdisplay x)
  (make-monad 'io (lambda () (display x) 'done)))

(define (run-io m)
  ((io-action
    (determine 'io
	       (lambda (io1 k) (k (run-io io1)))
	       (lambda (v) (make-monad 'io (lambda () v)))
	       m))))

(define state-xformer (monad-arg 'state))

(define sget
  (make-monad 'state (lambda (s0) (cons s0 s0))))

(define (sput a)
  (make-monad 'state (lambda (s0) (cons 'unit a))))

(define (run-st m initial)
  ((state-xformer (determine 'state
			     (lambda (st1 f)
			       (make-monad 'state
					   (lambda (s0)
					     (let* ((inp ((state-xformer st1) s0))
						    (v (car inp))
						    (s1 (cdr inp)))
					       (run-st (f v) s1)))))
			     (lambda (a)
			       (make-monad 'state
					   (lambda (s0) (cons a s0))))
			     m))
   initial))
