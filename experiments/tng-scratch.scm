(require (lib "9.ss" "srfi")
	 (lib "1.ss" "srfi")
	 "queue.ss")

(define *runq* (make-q))

(define (schedule thunk)
  (enq! *runq* thunk))

(define *throw-to-mainloop* '*throw-to-mainloop-not-initialised*)

(define (mainloop)
  (begin
    (call-with-current-continuation (lambda (cc) (set! *throw-to-mainloop* cc)))
    (if (q-empty? *runq*)
	(wait-for-events)
	(begin
	  ((deq! *runq*))
	  (mainloop)))))

(define (throw-to-mainloop)
  (*throw-to-mainloop* #f))

(define (wait-for-events) ;; %%%
  (display "Waiting for events.")
  (newline)
  (exit))

(define-record-type 

(define-record-type oop
  (make-oop outputs input slots)
  oop?
  (outputs oop-outputs set-oop-outputs!)
  (input oop-input set-oop-input!)
  (slots oop-slots set-oop-slots!))

(define-record-type input-handler
  (make-input-handler next proc datum)
  input-handler?
  (next input-handler-next)
  (proc input-handler-proc)
  (datum input-handler-datum))

(define *nil* '*nil*)

(define (primitive-new n)
  (make-oop (make-q) #f (make-vector n *nil*)))

(define (oop-ref o n)
  (vector-ref (oop-slots o) n))

(define (oop-set! o n x)
  (vector-set! (oop-slots o) n x))

(define (oop-length o)
  (vector-length (oop-slots o)))

(define (oop-send-full! oop message)
  (let ((handler (oop-input oop)))
    (if (not handler)
	(enq! (oop-outputs oop) message)
	(schedule (lambda () ((input-handler-proc handler)
			      (input-handler-datum handler)
			      #f
			      message))))))

;; Channel send
;;   - RPC service ready
;;     (A) - if (isa message <message>),
;;              call service with selector+args, collect result, send to continuation
;;              else ERROR <message> expected
;;   - reader ready
;;     (B) - schedule reader action
;;   - none ready
;;     (C) - enqueue message
;; Channel receive
;;   - RPC client ready
;;     - as for (B)
;;   - sender ready
;;     - as for (B)
;;   - none ready
;;     (D) - enqueue reader
;; RPC client
;;   - RPC service ready
;;     (E) - call service with selector+args, returning result directly
;;   - reader ready
;;     (F) - build message and schedule reader action
;;   - none ready
;;     (G) - build message and enqueue
;; RPC service
;;   - RPC client ready
;;     - as for (A)
;;   - sender ready
;;     - as for (A)
;;   - none ready
;;     (H) - enqueue RPC service

(define (oop-send-fast! oop selector argv)
  (let ((handler (oop-input oop)))
    (if (not handler)
	(call-with-current-continuation
	 (lambda (k)
	   (let ((message (make-message k selector argv)))
	     (enq! (oop-outputs oop) message)
	     (throw-to-mainloop))))
	((input-handler-proc handler)
	 (input-handler-datum handler)
	 selector
	 argv))))

(define (*restoring-handler* datum selector message)
  (set-oop-input! (vector-ref datum 0)
		  (vector-ref datum 1))
  ((vector-ref datum 2)
   (vector-ref datum 3)
   selector
   message))

(define (oop-hook-oneshot-input! oop handler-proc handler-datum)
  (let ((outputs (oop-outputs oop)))
    (if (q-empty? outputs)
	(let ((old-handler (oop-input oop)))
	  (set-oop-input! oop (make-input-handler *restoring-handler*
						  (vector oop
							  old-handler
							  handler-proc
							  handler-datum))))
	(let ((message (deq! outputs)))
	  (schedule
	  (handler-proc
	   handler-datum
	   #f
	   message)

(define (oop-hook-repeating-input! oop handler-proc handler-datum)
  (set-oop-input! oop (make-input-handler handler-proc
					  handler-datum)))
