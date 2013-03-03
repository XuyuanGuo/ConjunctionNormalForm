-- definition for Well Formed Formula
data Wff = Const Bool
	| Var Char
	| Not Wff
	| And Wff Wff
	| Or Wff Wff
	| Imply Wff Wff

-- my show functions
instance Show Wff where
	show (Var x) = [x]
	show (Not p) = "!" ++ show p
	show (And p q) = "(" ++ show p ++ "&" ++ show q ++ ")"
	show (Or p q) = "(" ++ show p ++ "|" ++ show q ++ ")"
	show (Imply p q) = "(" ++ show p ++ "-->" ++ show q ++ ")"

-- test case for implyFree()
p1 :: Wff
p1 = Imply (Var 'A') (Var 'B')
p2 :: Wff
p2 = Imply (And (Var 'A') (Not (Var 'B'))) (Var 'C')
p3 :: Wff
p3 = Not (Imply (Or (Var 'A') (Var 'B')) (Imply (And (Var 'C') (Var 'D')) (Var 'E')))
-- test case over
-- function implyFree(p)
-- precondition: p is a Wff
-- postcondition: implyFree(p) eliminates "Imply"s in p
implyFree (Var x) = Var x
implyFree (Not (Not p)) = implyFree p
implyFree (Not p) = Not (implyFree p)
implyFree (And p q) = And (implyFree p) (implyFree q)
implyFree (Or p q) = Or (implyFree p) (implyFree q)
implyFree (Imply p q) = Or (Not (implyFree p)) (implyFree q)

-- test case for transform()
p4 :: Wff
p4 = Or (Var 'A') (Var 'B')
p5 :: Wff
p5 = And (Or (Var 'A') (Var 'B')) (Not (Var 'C'))
p6 :: Wff
p6 = Not (Imply (Or (Var 'A') (Var 'B')) (Imply (Or (Var 'C') (Var 'D')) (Var 'E')))
-- test case over
-- function transform(p)
-- precondition: p is a Wff
-- postcondition: transform(p) eliminates "Or"s in p
transform (Var x) = Var x
transform (Not (Not p)) = transform p
transform (Not p) = Not (transform p)
transform (And p q) = And (transform p) (transform q)
transform (Or p q) = Not (And (transform p) (transform q))
transform (Imply p q) = Imply (transform p) (transform q)

-- test case for getNFF()
p7 :: Wff
p7 = Not (Not (Var 'A'))
p8 :: Wff
p8 = Not (And (Var 'A') (Var 'B'))
p9 :: Wff
p9 = Not (Or (And (Var 'A') (Var 'B')) (Not (Var 'C')))
-- test case over
-- function getNNF(p)
-- precondition: p is a Wff, and p has no "Imply"s
-- postcondition: getNNF(p) returns a Wff only negates atoms
getNNF (Var x) = Var x
getNNF (Not (Not p)) = getNNF p
getNNF (Not (Var x)) = Not (Var x)
getNNF (Not (And p q)) = Or (getNNF (Not p)) (getNNF (Not q))
getNNF (Not (Or p q)) = And (getNNF (Not p)) (getNNF (Not q))
getNNF (Or p q) = Or (getNNF p) (getNNF q)
getNNF (And p q) = And (getNNF p) (getNNF q)

-- test case for distr()
p10 :: Wff
p10 = And (Var 'A') (Var 'B')
p11 :: Wff
p11 = (Var 'C')
p12 :: Wff
p12 = Or (Var 'D') (Var 'E')
-- test case over
-- please test with "distr p10 p11" / "distr p11 p12" / "distr p12 p10"
-- function distr(p q)
-- precondition: p and q are in Conjunction Normal Form
-- postcondition: distr(p q) computes a CNF for p|q
distr (And p q) r = And (distr p r) (distr q r)
distr r (And p q) = And (distr r p) (distr r q)
distr p q = Or p q

-- test case for getCNF()
p13 :: Wff
p13 = (Var 'A')
p14 :: Wff
p14 = And (Var 'A') (Var 'B')
p15 :: Wff
p15 = Or p14 (Not (Var 'C'))
p16 :: Wff
p16 = Or p15 p14
-- test case over
-- function getCNF(p)
-- precondition: p is an implication free and is in NNF
-- postcondition: getCNF(p) returns the CNF for p
getCNF (Var x) = Var x
getCNF (Not p) = Not (getCNF p)
getCNF (And p q) = And (getCNF p) (getCNF q)
getCNF (Or p q) = distr (getCNF p) (getCNF q)
