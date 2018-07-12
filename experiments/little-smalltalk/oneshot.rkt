#lang racket

(provide oneshot oneshot-set! oneshot-ref)

(define (oneshot)
  (thread (lambda ()
            (define (no-value waiters)
              (match (thread-receive)
                [(list 'get ch) (no-value (cons ch waiters))]
                [(list 'set v)
                 (for [(ch waiters)] (channel-put ch v))
                 (value v)]))
            (define (value v)
              (match (thread-receive)
                [(list 'get ch)
                 (channel-put ch v)
                 (value v)]
                [(list 'set v)
                 (value v)]))
            (no-value '()))))

(define (oneshot-set! o v)
  (thread-send o (list 'set v)))

(define (oneshot-ref o)
  (define ch (make-channel))
  (thread-send o (list 'get ch))
  (channel-get ch))
