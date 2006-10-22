;;; justlazy.scm
;; Cut-down version of lazy2 srfi-45 implementation by Eli Barzilay.
;; See message in post-finalization srfi-45 archive, 20 Oct 2006
;; 03:08:37 -0400 with subject "Re: Simpler implementation"

(module justlazy mzscheme
  (provide lazy unlazy lazy?) ;; unlazy <- force, lazy? <- promise?

  (define-struct lazy-promise (p))

  (define lazy? lazy-promise?)

  (define-syntax lazy
    (syntax-rules ()
      ((lazy exp) (make-lazy-promise (lambda () exp)))))

  (define (unlazy promise)
    (let ((p (lazy-promise-p promise)))
      (cond ((procedure? p)	(set-lazy-promise-p! promise #f) ; mark root for cycle detection
				(let loop ((promise* (p)))
				  (if (lazy-promise? promise*)
				      (let ((p* (lazy-promise-p promise*)))
					(set-lazy-promise-p! promise* promise) ; share with root
					(cond ((procedure? p*) (loop (p*)))
					      ((pair? p*) (set-lazy-promise-p! promise p*)
							  (car p*))
					      ((lazy-promise? p*) (loop p*))
					      ((not p*) (error "Reentrant promise"))
					      (else (error "Invalid promise, contains" p*))))
				      (begin ;; error here for "library approach"
					     (set-lazy-promise-p! promise (list promise*))
					     promise*))))
            ((pair? p)		(car p))
            ((lazy-promise? p)	(unlazy p))
            ((not p)		(error "Reentrant promise"))
            (else		(error "Invalid promise, contains" p))))))
