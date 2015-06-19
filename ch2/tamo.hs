-- implication
infix 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

-- True ==> x  = x
-- False ==> x = True

-- equivilence p => q and q => p
infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

-- exclusive or
infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y


p = True
q = False

formula1 = (not p) && (p ==> q) <=> not (q && (not p))

