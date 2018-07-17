#lang racket
;; Crude measurement of techniques for patching functions during runtime.
;;
;; 2018-07-17 13:25:34 With Racket v6.90, unboxing is superior to
;; case-lambda; unboxing is only about 5% slower than not being able
;; to update the function at all.

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define epoch 0)

(define (main)
  (define N 50000000)

  (define (by-immutable)
    (define f (eval '(lambda (x) (+ x 1)) ns))
    (for/fold [(x 0)] [(n (in-range N))] (f x)))

  ;; (printf "by-immutable\n")
  ;; (for [(i 5)] (time (by-immutable)))
  ;; (newline)

  (define (by-unboxing)
    (define f (eval '(box (lambda (x) (+ x 1))) ns))
    (for/fold [(x 0)] [(n (in-range N))] ((unbox f) x)))

  (printf "by-unboxing\n")
  (for [(i 5)] (time (by-unboxing)))
  (newline)

  (define (by-embedding)
    (define f (eval '(let ((inner-f (lambda (x) (+ x 1))))
                       (case-lambda
                         [() (lambda (new-f) (set! inner-f new-f))]
                         [(x) (inner-f x)]))
                    ns))
    (for/fold [(x 0)] [(n (in-range N))] (f x)))

  ;; (printf "by-embedding\n")
  ;; (for [(i 5)] (time (by-embedding)))
  ;; (newline)

  (define (by-unboxing-with-check)
    (define f (eval `(letrec ((b (box (lambda (x)
                                        (if (> epoch ,epoch)
                                            (begin (set-box! b (lambda (x) (+ x 2)))
                                                   ((unbox b) x))
                                            (+ x 1))))))
                       b)
                    ns))
    (for/fold [(x 0)] [(n (in-range N))] ((unbox f) x)))

  (printf "by-unboxing-with-check\n")
  (for [(i 5)] (time (by-unboxing-with-check)))
  (newline)

  ;; TODO: Experiment with hoisting the check out to being on the send
  ;; side, rather than the receive side; also consider the idea that
  ;; when inlining/partially-evaluating, probably want to residualize
  ;; the epoch check for each inlined method! If there's a separate
  ;; epoch for each method, that is. (If there's a global epoch,
  ;; everything gets invalidated at once, and the check just needs to
  ;; live at the top of each method - or every box has to be
  ;; discoverable and updateable.) Ultimately, there's some
  ;; interesting interaction between the source-ish forms of the
  ;; method and the target-ish forms of the method here.
  (define (by-unboxing-with-check-but-no-fixed-box)
    (define f (eval `(box (lambda (b x)
                            (if (> epoch ,epoch)
                                (begin (set-box! b (lambda (x) (+ x 2)))
                                       ((unbox b) b x))
                                (+ x 1))))
                    ns))
    (for/fold [(x 0)] [(n (in-range N))] ((unbox f) f x)))

  (printf "by-unboxing-with-check-but-no-fixed-box\n")
  (for [(i 5)] (time (by-unboxing-with-check-but-no-fixed-box)))
  (newline)

  )

(module+ main (main))
