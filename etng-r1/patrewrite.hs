module PatRewrite where

import qualified Maybe

type Literal = String -- good enough for now

type Varname = String

data ConcretePat = CAnd ConcretePat ConcretePat
                 | CDiscard
                 | CBinding Varname
                 | CTuple [ConcretePat]
                 | CLiteral Literal
                   deriving Show

data Value = VLiteral Literal
           | VTuple [Value]
             deriving Show

-- Pattern matcher:
--   v     -> sk                        -> fk        -> result
--   Value -> (Bindings -> a)           -> (() -> a) -> a
--   Value -> ([(Varname, Value)] -> a) -> (() -> a) -> a

-- data MatchTree = MDiscard Outcome
--                | MBinding Varname Outcome
--                | MTuple Integer Outcome
--                | MLiteral Literal Outcome
--                | MOr [MatchTree]

names (CAnd l r) = names l ++ names r
names (CDiscard) = []
names (CBinding n) = [n]
names (CTuple ps) = concatMap names ps
names (CLiteral l) = []

match (CAnd l r) v sk fk = match l v (\lbs -> match r v (\rbs -> sk (lbs ++ rbs)) fk) fk
match (CDiscard) v sk fk = sk []
match (CBinding n) v sk fk = sk [(n, v)]
match (CTuple ps) v sk fk = matchTuple ps v sk fk
match (CLiteral l) v sk fk = matchLiteral l v sk fk

matchLiteral pl (VLiteral vl) sk fk = if vl == pl then sk [] else fk ()
matchLiteral p  (VTuple _) sk fk = fk ()

matchTuple ps (VLiteral _) sk fk = fk ()
matchTuple ps (VTuple vs) sk fk = matchTuple' ps vs sk fk

matchTuple' [] [] sk fk = sk []
matchTuple' (p:ps) (v:vs) sk fk = match p v (\bs -> matchTuple' ps vs (\bss -> sk (bs ++ bss)) fk) fk
matchTuple' _ _ sk fk = fk ()

-- We want a compiled matcher to be of type
-- Value -> Maybe ([Value], ClosureBody, ClosureEnv)

-- (a, (b1,b2)#(c,_))
t1 = CTuple [CBinding "a",
             CAnd (CTuple [CBinding "b1", CBinding "b2"])
                  (CTuple [CBinding "c", CDiscard])]

v1 = VTuple [VLiteral "x",
             VTuple [VLiteral "y", VLiteral "z"]]

-- a#(_,_)
t2 = CAnd (CBinding "a") (CTuple [CDiscard, CDiscard])
