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

;; Boudol's Blue Calculus
;;---------------------------------------------------------------------------

;; P = A | D | (P|P) | (u)P
;; A = u | \u.P | Pu
;; D = u:=P | u==P
;; E = [] | Eu | (E|P) | (P|E) | (u)E

;; STRUCTURAL EQUIVALENCE
;;---------------------------------------------------------------------------
;; equivalence, containing alpha-conversion
;; par commutative, associative
;; (u)P | Q  ===  (u)(P | Q)  when u not free in Q
;;   (P|Q)u  ===  (Pu | Qu)
;;  ((u)P)v  ===  (u)(Pv)     when u <> v
;;       Du  ===  D
;;     u==P  ===  u:=(P | u==P)
;; equivalence of processes implies equivalence of evaluation contexts E

;; REDUCTION
;;---------------------------------------------------------------------------
;;  (\u.P)v  -->  P{v/u}      small beta reduction
;; u | u:=P  -->  P           rho - resource fetching
;;
;; P-->P' => E[P]-->E[P']     context
;;
;; P-->P' & Q===P => Q-->P'   structural

