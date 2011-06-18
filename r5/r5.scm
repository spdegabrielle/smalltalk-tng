; Embedding, to explore semantics

(define-language Void
  void)

(define-language Bool
  true
  false)

(define-language LogicalOperators
  not
  (&& ,@other)
  (|| ,@other))

(define-language Maybe
  nothing
  (just ,x))

(define-language Project
  (as ,language success ,in failure ,out))

(define-language IfMenu
  (ifTrue ,@body)
  (ifFalse ,@body)
  (ifTrue ,@body ifFalse ,@body)
  (ifFalse ,@body ifTrue ,@body))

(define id (x -> x))
(define always (v -> (x -> v)))

(define case (message -> (dispatcher -> (dispatcher message))))

(define-extension false
  (not			-> true)
  ((&& f)		-> false)
  ((|| f)		-> (f))
  ((ifTrue b)		-> void)
  ((ifFalse b)		-> (b))
  ((ifTrue t ifFalse f) -> (f))
  ((ifFalse f ifTrue t) -> (f)))

(define-extension true
  (not			-> false)
  ((&& f)		-> (f))
  ((|| f)		-> true)
  ((ifTrue b)		-> (b))
  ((ifFalse b)		-> void)
  ((ifTrue t ifFalse f) -> (t))
  ((ifFalse f ifTrue t) -> (t)))

(define isJust
  (rec (nothing  -> false)
       ((just x) -> true)))

(define fromJust
  (rec (nothing  -> (error "Nothing found"))
       ((just x) -> x)))

(define-language Enumerable
  (next ,k)
  head
  tail
  isEmpty
  (inject ,knil into ,kons)
  length
  (do ,f))

(define-language Stream
  done
  (yield ,value ,tail))

(define-extension done self
  ((next k) -> (k self)))

(define-extension (yield _ _) self
  ((next k) -> (k self)))

(define ENUMERABLE
  (rec self
       (head ->
	 (self next (rec (done -> (error "head of empty enumerable")) ((yield v s) -> v))))
       (tail ->
	 (self next (rec (done -> (error "tail of empty enumerable")) ((yield v s) -> s))))
       (isEmpty ->
	 (self next (rec (done -> true) ((yield _ _) -> false))))
       ((inject knil into kons) ->
	 (self next (rec (done -> knil) (yield v s -> (s (inject (kons v knil) into kons))))))
       (length ->
	 (self (inject 0 into (_ -> (a -> (+ a 1))))))
       ((do f) ->
	 (self (inject done into (v -> (a -> (begin (f v) a))))))))
