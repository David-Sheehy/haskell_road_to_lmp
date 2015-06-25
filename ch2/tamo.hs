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
formula2 p q = ((not p) && (p ==> q) <=> not (q && (not p)))


valid1 :: (Bool -> Bool) -> Bool
valid1 bf = (bf True) && bf(False)


excluded_middle :: Bool -> Bool
excluded_middle p = p || not p


valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf =    (bf True True)
            && (bf True False)
            && (bf False True)
            && (bf False False)

form1 p q = p ==> (q ==> p)
form2 p q = (p ==> q) ==> p

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [ bf p q r | p <- [True, False],
                             q <- [True, False],
                             r <- [True, False]]


valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [bf p q r s | p <- [True, False],
                              q <- [True, False],
                              r <- [True, False],
                              s <- [True, False]]


logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 = (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)


logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and [(bf1 p q) <=> (bf2 p q) | p <- [True, False],
                                                   q <- [True, False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = and [(bf1 p q r) <=> (bf2 p q r) | p <- [True, False],
                                                       q <- [True, False],
                                                       r <- [True, False]]


-- exercise 2.9
formula3 p q = p
formula4 p q = (p <+> q) <+> q
formula5 p q = p <=> ((p <+> q) <+> q)  -- formula3 equivilent to formula3

-- exercise 2.13
-- Checks for theorem 2.12

check1 = not True <=> False
check2 = not False <=> True
check3 :: Bool
check3 = logEquiv1 (\ p -> p ==> False ) (\ p -> not p)

-- exercise 2.15
-- Write contradiction tests for propositional functions with one, two and
-- three variables
contra1 :: (Bool -> Bool) -> Bool
contra1 bf = not (or [ bf p | p <- [True, False] ]) 

contra2 :: (Bool ->Bool -> Bool) -> Bool
contra2 bf = not (or [ bf p q | p <- [True, False],
                                q <- [True, False] ]) 

contra3 :: (Bool -> Bool ->Bool -> Bool) -> Bool
contra3 bf = not (or [ bf p q r | p <- [True, False],
                                  q <- [True, False],
                                  r <- [True, False] ])  
