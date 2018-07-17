#lang racket
;; Crude measurement of techniques for patching functions during runtime.
;;
;; 2018-07-17 13:25:34 With Racket v6.90, unboxing is superior to
;; case-lambda; unboxing is only about 5% slower than not being able
;; to update the function at all.

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define (main)
  (define N 50000000)

  (define (by-immutable)
    (define f (eval '(lambda (x) (+ x 1)) ns))
    (for/fold [(x 0)] [(n (in-range N))] (f x)))

  (for [(i 5)] (time (by-immutable)))
  (newline)

  (define (by-unboxing)
    (define f (eval '(box (lambda (x) (+ x 1))) ns))
    (for/fold [(x 0)] [(n (in-range N))] ((unbox f) x)))

  (for [(i 5)] (time (by-unboxing)))
  (newline)

  (define (by-embedding)
    (define f (eval '(let ((inner-f (lambda (x) (+ x 1))))
                       (case-lambda
                         [() (lambda (new-f) (set! inner-f new-f))]
                         [(x) (inner-f x)]))
                    ns))
    (for/fold [(x 0)] [(n (in-range N))] (f x)))

  (for [(i 5)] (time (by-embedding)))
  (newline))

(module+ main (main))
