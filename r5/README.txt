Characterised by language definitions describing data,
pattern-matching functions describing codata, protocol for presenting
codata as data, and lexically-scoped "reifiers" for presenting data as
codata.

Multiple namespaces:
 - constructors = macros
 - values

The reason for the multiple namespaces is so that the constructors and
destructors are known to the compiler, so it can figure out (parse)
message construction (and destruction) token sequences.

NOTE re streams/enumeration below: can dispense with "next :k" because
codata and data can project themselves directly into instances of
done/yield!

NOTE good running examples: samefringe and DSL for email handling (or
I guess HTTP, sigh)

Permit .field accessors to data?


language Void
  | void

language List
  | nil
  | :head # :tail (right)

language Bool
  | true
  | false

language LogicalOperators
  | not
  | && ?:other
  | || ?:other

language Maybe
  | nothing
  | just :x

language Project
  | as :language success :in failure :out

def id = x -> x
def always = v -> x -> v

def case = message -> dispatcher -> dispatcher message
def if = test -> continuations -> continuations test

extend false
  | not -> true
  | && f -> false
  | || f -> f void

extend true
  | not -> false
  | && f -> f void
  | || f -> true

def isJust =
  | nothing -> false
  | just _ -> true

def fromJust =
  | nothing -> error "Nothing found"
  | just x -> x

language Enumerable =
  | next :k
  | head
  | tail
  | isEmpty
  | inject :knil into :kons
  | length
  | do :f

language Stream =
  | done
  | yield :value :tail

def ENUMERABLE =
  | @self head -> self next ( done -> error "head of empty enumerable" | yield v s -> v)
  | @self tail -> self next ( done -> error "tail of empty enumerable" | yield v s -> s)
  | @self isEmpty -> self next ( done -> true | yield _ _ -> false )
  | @self inject knil into kons ->
      self next ( done -> knil | yield v s -> s inject (kons v knil) into kons )
  | @self length -> self inject 0 into ( _ a -> a + 1 )
  | @self do f -> self inject done into ( v a -> begin f v; a end )
