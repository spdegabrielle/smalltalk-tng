;; Unifying pattern-matching, parsing, and method dispatch

(define (alt p1 p2)
  (lambda (msg kt kf)
    (p1 msg
	kt
	(lambda () (p2 msg kt kf)))))

;; { .foo (x, 2, y) -> .ok }
;;
;; seq(literal("foo"), seq(tuple(3, [bind(x, discard()), literal(2), bind(y, discard())]),
;;                         cut( ... )))

(define (p . xs)
  (write xs)
  (newline)
  (last xs))

(define (seq p1 p2)
  (lambda (msg kt kf)
    (p 'seq msg)
    (if (null? msg)
	(kt (seq p1 p2) msg)
	(p1 (car msg)
	    (lambda (sv remainder) (p2 (cdr msg) kt kf))
	    kf))))

(define (empty-seq)
  (literal '()))

(define (tuple n pats)
  (let ((tp (fold-right seq (empty-seq) pats)))
    (lambda (msg kt kf)
      (p `(tuple ,n) msg)
      (if (and (vector? msg)
	       (= (vector-length msg) n))
	  (tp (vector->list msg) kt kf)
	  (kf)))))

(define (discard)
  (lambda (msg kt kf)
    (p 'discard msg)
    (kt msg '())))

(define (literal v)
  (lambda (msg kt kf)
    (p `(literal ,v) msg)
    (if (eqv? msg v)
	(kt msg '())
	(kf))))

(define (cut obj-producer-thunk)
  (lambda (msg kt kf)
    (p 'cut msg)
    (kt (obj-producer-thunk) msg)))

(define (fail)
  (lambda (msg kt kf)
    (p 'fail msg)
    (kf)))


(define (feed parser msg)
  (parser msg
	  (lambda (sv remainder)
	    (p 'SUCCESS sv remainder)
	    (if (null? remainder)
		'complete
		(feed sv remainder)))
	  (lambda ()
	    (p 'FAILURE)
	    'incomplete)))


;; { .x -> { .y -> 1 } ;
;;   .x -> { .z -> 2 } ;
;;   .z -> {  _ -> 3 } }

(define (t)
  (alt (seq (literal 'x) (cut (lambda () (seq (literal 'y) (cut (lambda () 1))))))
       (alt (seq (literal 'x) (cut (lambda () (seq (literal 'z) (cut (lambda () 2))))))
	    (alt (seq (literal 'z) (cut (lambda () (seq (discard) (cut (lambda () 3))))))
		 (fail)))))

;; { .x .y -> 1 ;
;;   .x .z -> 2 ;
;;   .z  _ -> 3 }

(define (t2)
  (alt (seq (literal 'x) (seq (literal 'y) (cut (lambda () 1))))
       (alt (seq (literal 'x) (seq (literal 'z) (cut (lambda () 2))))
	    (alt (seq (literal 'z) (seq (discard) (cut (lambda () 3))))
		 (fail)))))

(define (drive name parser msg)
  (p '------------DRIVE name msg)
  (p '--> (feed parser msg))
  (newline))

(define (drive4 name parser)
  (drive name parser '(x y))
  (drive name parser '(x z))
  (drive name parser '(x x))
  (drive name parser '(z z)))

(define (run)
  (drive4 't (t))
  (drive4 't2 (t2)))
