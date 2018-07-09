#lang racket
;; 2018-07-09 14:05:49 Racket translation of August 2009's precedence-parsing.scm.

(define table '((== non 4)
		(: right 5)
		(++ right 5)
		(+ left 6)
		(- left 6)
		(* left 7)
		(/ left 7)))

(define (parse exp)
  (define (p-op lhs exp min-precedence k)
    (match exp
      ['() (k lhs exp)]
      [(cons op rest0)
       (match (assq op table)
         [(list _op fixity prec)
          (if (>= prec min-precedence)
              (match-let ([(cons rhs rest) rest0])
                (let loop ((rhs rhs)
                           (rest rest))
                  (match rest
                    ['() (k `(,op ,lhs ,rhs) rest)]
                    [(cons lookahead rest1)
                     (match (assq lookahead table)
                       [(list _lookahead lfixity lprec)
                        (if (or (and (eq? lfixity 'right)
                                     (= lprec prec))
                                (> lprec prec))
                            (p-op rhs rest lprec loop)
                            (p-op `(,op ,lhs ,rhs) rest min-precedence k))]
                       [#f (loop `(app ,rhs ,lookahead) rest1)])])))
              (k lhs exp))]
         [#f (p-op `(app ,lhs ,op) rest0 min-precedence k)])]))

  (match-let ([(cons lhs rest) exp])
    (p-op lhs rest 0 (lambda (result rest)
                       (list 'result! result rest)))))

(write (parse '(1 + 2 + 2.5 : 3 * 4 y z * 6 : a : b : c)))
;;(write (parse '(1 + 2 : 3 * 4 * 6 : foo)))
(newline)

