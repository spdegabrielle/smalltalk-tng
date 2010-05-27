Started from etng-r2, with objcap and 3-move in mind.

No quoted symbols.

Pattern clauses are expressions, evaluated at closure construction
time (?) containing embedded pattern-variable *values*. Application
invokes matchValue() on a pattern.

Parenthesised like scheme.

[a b c] = (seq a b c)

{:a :b -> v} = (lambda [:a :b] v)

{ + :n -> (n + self)
  - :n -> (n - self) } = (lambda [+ :n] (n + self)
				 [- :n] (n - self))

{print n}

(fn -> (begin (print n) (print m)))
{(begin (print n) (print m))}
{begin (print n) (print m)}

(rec addOne :a -> (a + 1)
     addTwo :b -> (b + 2))

(fn addOne :a -> (a + 1)
    addTwo :b -> (b + 2))

(define seq { || :a -> a })

(define (seq || :a) a)

(define ns (make-namespace)) ;; fresh, anonymous, generative namespace
(define n2 (make-namespace "http://some.name/space"))

(define-symbols matchValue someOther otherThing) ;; now can use these variables as pseudoliterals
(define-symbol matchValue) ;; in a fresh, anonymous, generative namespace for this module
(define-symbol matchValue "http://some.name/space") ;; in a well-known namespace

;; A module
(define-module "my-module" (platform)
  (let ((strings (platform "strings"))
	(math (platform "math")))
    ...))

;; Anonymous module
(module (platform)
    ...)

Variants:
 - . instead of || for remainder-of-sequence
 - (fn [...] ... [...] ...) instead of {... -> ... ... -> ...}
 - (rec :self pat val pat val ...)
 - have symbol namespaces be the module's name

How to define the pipeline syntax? (a | extends b)

How to avoid parens in long chain unary message sequences?

** Problems with nullary function definitions and calls! **

