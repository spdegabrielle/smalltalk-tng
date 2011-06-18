(define-language Term
  (num :value)
  (var :name)
  (:left times :right))

;; Lame
(define simplify
  (rec ((x times (num 1)) -> x)
       (v -> v)))

;; Better
(define simplify
  (rec ((x times (num 1)) -> (simplify x))
       (((num 1) times x) -> (simplify x))
       (v -> v)))
