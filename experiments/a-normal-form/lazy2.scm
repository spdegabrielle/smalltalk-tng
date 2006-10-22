;; lazy2.scm -- srfi-45 implementation by Eli Barzilay
;; See message in post-finalization srfi-45 archive, 20 Oct 2006 03:08:37 -0400
;; with subject "Re: Simpler implementation"

(module lazy2 mzscheme-no-promises

  (provide lazy delay force promise?)

  (define-struct promise (p))
  ;; <promise> ::= (make-promise <thunk of promise>) (delayed promise)
  ;;             | (make-promise (list <object>))    (forced promise)
  ;;             | (make-promise <promise>)          (shared promise)
  ;;             | (make-promise #f)                 (current running promise)

  (define-syntax lazy
    (syntax-rules ()
      [(lazy exp) (make-promise (lambda () exp))]))

  (define-syntax delay
    (syntax-rules ()
      [(delay exp) (lazy (make-promise (list exp)))]))

  (define (force promise)
    (let ([p (promise-p promise)])
      (cond [(procedure? p)
             (set-promise-p! promise #f) ; mark root for cycle detection
             (let loop ([promise* (p)])
               (if (promise? promise*)
                 (let ([p* (promise-p promise*)])
                   (set-promise-p! promise* promise) ; share with root
                   (cond [(procedure? p*) (loop (p*))]
                         [(pair? p*) (set-promise-p! promise p*)
                                     (car p*)]
                         [(promise? p*) (loop p*)]
                         [(not p*) (error "Reentrant promise")]
                         [else (error "Invalid promise, contains" p*)]))
                 (begin ;; error here for "library approach"
                        (set-promise-p! promise (list promise*))
                        promise*)))]
            [(pair? p)    (car p)]
            [(promise? p) (force p)]
            [(not p)      (error "Reentrant promise")]
            [else         (error "Invalid promise, contains" p)]))))
