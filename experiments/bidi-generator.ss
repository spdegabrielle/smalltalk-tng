#lang scheme

(provide yield generator)

(define current-yielder
  (make-parameter
   (lambda (v)
     (error 'yield "must be called in the context of a generator"))))

(define (yield . vals)
  (apply (current-yielder) vals))

(define-syntax-rule (generator init-formals body0 body ...)
  (letrec ((yielder (lambda vals
                      (call-with-current-continuation
                         (lambda (new-inner-k)
                           (set! inner-k new-inner-k)
                           (apply outer-k vals)))))
           (outer-k #f)
           (inner-k (lambda init-formals
                      (call-with-values
                       (lambda ()
                         (parameterize ((current-yielder yielder))
                           body0 body ...))
                       (lambda results
                         (set! inner-k (lambda ignored (error "Generator is terminated")))
                         (apply outer-k results))))))
    (lambda args
      (call-with-current-continuation
       (lambda (new-outer-k)
         (set! outer-k new-outer-k)
         (apply inner-k args))))))
