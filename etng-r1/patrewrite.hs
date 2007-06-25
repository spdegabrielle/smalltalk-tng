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

names (CAnd l r) = names l ++ names r
names (CDiscard) = []
names (CBinding n) = [n]
names (CTuple ps) = concatMap names ps
names (CLiteral l) = []

match v (CAnd l r) sk fk = match v l (\lbs -> match v r (\rbs -> sk (lbs ++ rbs)) fk) fk
match v (CDiscard) sk fk = sk []
match v (CBinding n) sk fk = sk [(n, v)]
match v (CTuple ps) sk fk = matchTuple v ps sk fk
match v (CLiteral l) sk fk = matchLiteral v l sk fk

matchLiteral (VLiteral vl) pl sk fk = if vl == pl then sk [] else fk
matchLiteral (VTuple _) p sk fk = fk

matchTuple (VLiteral _) ps sk fk = fk
matchTuple (VTuple vs) ps sk fk = matchTuple' vs ps sk fk

matchTuple' [] [] sk fk = sk []
matchTuple' (v:vs) (p:ps) sk fk = match v p (\bs -> matchTuple' vs ps (\bss -> sk (bs ++ bss)) fk) fk
matchTuple' _ _ sk fk = fk

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
