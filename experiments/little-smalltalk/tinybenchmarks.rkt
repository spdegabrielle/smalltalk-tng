#lang racket
;; Sends-per-second benchmarking. 0 tinyBenchmarks computes "sends per
;; second" by taking the result of a call to benchFib and dividing it
;; by the time it took. So for (benchfib 28), yielding 1028457, if it
;; took 0.5 seconds, you'd have "sends/sec" of 2056914.

(module+ test (require rackunit))

(define (benchfib n)
  (if (< n 2)
      1
      (+ (benchfib (- n 1))
         (benchfib (- n 2))
         1)))

(module+ test
  (check-equal? (benchfib 1) 1)
  (check-equal? (benchfib 10) 177))

(define f< <)
(define f+ +)
(define f- -)

(define kont (make-parameter '()))

(define-syntax-rule (with-k x b)
  (parameterize ((kont (cons x (kont)))) b))

(define int%
  (class object%
    (define/public (benchfib/k n)
      (if (with-k 'if (< n 2))
          1
          (+ (with-k 'b1 (send this benchfib/k (with-k '-1 (- n 1))))
             (with-k 'add (+ (with-k 'b2 (send this benchfib/k (with-k '-2 (- n 2))))
                             1)))))

    (define/public (benchfib* n k)
      (send this <* n 2
            (lambda (is-lt-2)
              (if is-lt-2
                  (k 1)
                  (send this -* n 1
                        (lambda (n1)
                          (send this benchfib* n1
                                (lambda (v1)
                                  (send this -* n 2
                                        (lambda (n2)
                                          (send this benchfib* n2
                                                (lambda (v2)
                                                  (send this +* v1 v2
                                                        (lambda (v3)
                                                          (send this +* v3 1 k)))))))))))))))

    (define/public (benchfib n)
      (if (send this < n 2)
          1
          (send this +
                (send this benchfib (send this - n 1))
                (send this +
                      (send this benchfib (send this - n 2))
                      1))))

    (define/public (benchfib** n)
      (if (f< n 2)
          1
          (f+ (send this benchfib** (f- n 1))
              (send this benchfib** (f- n 2))
              1)))

    (define/public (<* a b k) (k (< a b)))
    (define/public (+* a b k) (k (+ a b)))
    (define/public (-* a b k) (k (- a b)))

    (define/public (< a b) (f< a b))
    (define/public (+ a b) (f+ a b))
    (define/public (- a b) (f- a b))

    (super-new)))

(module+ test
  (check-equal? (send (make-object int%) benchfib 1) 1)
  (check-equal? (send (make-object int%) benchfib 10) 177)
  (check-equal? (send (make-object int%) benchfib* 10 values) 177)
  (check-equal? (send (make-object int%) benchfib** 10) 177)
  (check-equal? (send (make-object int%) benchfib/k 10) 177))

(define-syntax-rule (time-benchfib expr)
  (let-values (((results cpu-ms wall-ms gc-ms) (time-apply (lambda () expr) '())))
    (match-define (list result) results)
    (printf "~a returned ~a in ~a CPU-ms (~a without GC time) -> ~a sends/sec\n"
            'expr
            result
            cpu-ms
            (- cpu-ms gc-ms)
            (/ result (/ cpu-ms 1000.0)))
    (flush-output)))

(time-benchfib (benchfib 28))
(time-benchfib (send (make-object int%) benchfib 28))
(time-benchfib (send (make-object int%) benchfib* 28 values))
(time-benchfib (send (make-object int%) benchfib** 28))
(time-benchfib (send (make-object int%) benchfib/k 28))
