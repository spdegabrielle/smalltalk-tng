(require srfi/9)

(print-struct #t)

(define previous-inspector (current-inspector))
(current-inspector (make-inspector))

(define-record-type stream
  (make-stream stepper state)
  stream?
  (stepper stream-stepper)
  (state stream-state))

(define-record-type done
  (make-done)
  done?)

(define-record-type skip
  (make-skip next-state)
  skip?
  (next-state skip-next-state))

(define-record-type yield
  (make-yield value next-state)
  yield?
  (value yield-value)
  (next-state yield-next-state))

(define-record-type nothing
  (make-nothing)
  nothing?)

(define-record-type just
  (make-just value)
  just?
  (value just-value))

(current-inspector previous-inspector)

;;---------------------------------------------------------------------------

(define (next s)
  ((stream-stepper s) (stream-state s)))

(define (replace-state str s)
  (make-stream (stream-stepper str) s))

;;---------------------------------------------------------------------------
;; Stream constructors

(define (return x)
  (make-stream (lambda (state)
		 (if state
		     (make-yield x #f)
		     (make-done)))
	       #t))

(define (range low high)
  (make-stream (lambda (state)
		 (if (< state high)
		     (make-yield state (+ state 1))
		     (make-done)))
	       low))

;;---------------------------------------------------------------------------
;; Stream consumers, leading to values not (necessarily) of type stream

(define (head s)
  (let ((step (next s)))
    (cond
     ((done? step) (error "head of empty stream"))
     ((skip? step) (head (skip-next-state step)))
     ((yield? step) (yield-value step)))))

(define (tail s)
  (let ((step (next s)))
    (cond
     ((done? step) (error "tail of empty stream"))
     ((skip? step) (tail (skip-next-state step)))
     ((yield? step) (replace-state s (yield-next-state step))))))

;; head-and-tail ?

(define (foldr s knil kons)
  (let go ((state (stream-state s)))
    (let ((step ((stream-stepper s) state)))
      (cond
       ((done? step) knil)
       ((skip? step) (go (skip-next-state step)))
       ((yield? step) (f (yield-value step) (go (yield-next-state step))))))))

(define (foldl s knil kons)
  (let go ((knil knil) (state (stream-state s)))
    (let ((step ((stream-stepper s) state)))
      (cond
       ((done? step) knil)
       ((skip? step) (go knil (skip-next-state step)))
       ((yield? step) (go (f (yield-value step) knil) (yield-next-state step)))))))

;;---------------------------------------------------------------------------
;; Stream transformers

(define (map s f)
  (make-stream (lambda (state)
		 (let ((step (next s)))
		   (cond
		    ((done? step) step)
		    ((skip? step) step)
		    ((yield? step) (make-yield (f (yield-value step)) (yield-next-state step))))))
	       (stream-state s)))

(define (append s1 s2)
  (make-stream (lambda (state)
		 (case (car state)
		   ((left)
		    (let ((step ((stream-stepper s1) (cdr state))))
		      (cond
		       ((done? step) (make-skip (cons 'right (stream-state s2))))
		       ((skip? step) (make-skip (cons 'left (skip-next-state step))))
		       ((yield? step) (make-yield (yield-value step)
						  (cons 'left (yield-next-state step)))))))
		   ((right)
		    (let ((step ((stream-stepper s2) (cdr state))))
		      (cond
		       ((done? step) (make-done))
		       ((skip? step) (make-skip (cons 'right (skip-next-state step))))
		       ((yield? step) (make-yield (yield-value step)
						  (cons 'right (yield-next-state step)))))))))
	       (cons 'left (stream-state s1))))

(define (zip s1 s2)
  (make-stream (lambda (state)
		 (let ((state1 (car state))
		       (state2 (cadr state))
		       (latch (caddr state)))
		   (cond
		    ((nothing? latch)
		     (let ((step ((stream-stepper s1) state1)))
		       (cond
			((done? step) (make-done))
			((skip? step) (make-skip (list (skip-next-state step)
						       state2
						       (make-nothing))))
			((yield? step) (make-skip (list (yield-next-state step)
							state2
							(make-just (yield-value step))))))))
		    ((just? latch)
		     (let ((step ((stream-stepper s1) state1)))
		       (cond
			((done? step) (make-done))
			((skip? step) (make-skip (list state1
						       (skip-next-state step)
						       latch)))
			((yield? step) (make-yield (list (just-value latch)
							 (yield-value step))
						   (list state1
							 (yield-next-state step)
							 (make-nothing))))))))))
	       (list (stream-state s1)
		     (stream-state s2)
		     (make-nothing))))

(define (concatmap s f)
  (make-stream (lambda (state)
		 (let ((outerstate (car state))
		       (latch (cdr state)))
		   (cond
		    ((nothing? latch)
		     (let ((step ((stream-stepper s) outerstate)))
		       (cond
			((done? step) (make-done))
			((skip? step) (make-skip (cons (skip-next-state step) latch)))
			((yield? step) (make-skip (cons (yield-next-state step)
							(make-just (f (yield-value step)))))))))
		    ((just? latch)
		     (let* ((inners (just-value latch))
			    (step (next inners)))
		       (cond
			((done? step) (make-skip (cons outerstate (make-nothing))))
			((skip? step) (make-skip (cons outerstate
						       (make-just
							(replace-state inners
								       (skip-next-state step))))))
			((yield? step) (make-yield (yield-value step)
						   (cons outerstate
							 (make-just
							  (replace-state inners
									 (yield-next-state
									  step))))))))))))
	       (cons (stream-state s) (make-nothing))))
