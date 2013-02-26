--parsing input
data UOperator = Sqrt | Sin | Cos | Ln | Exp deriving(Enum, Eq)
data BOperator = Sum | Diff | Div | Mul | Pow deriving(Enum, Eq)
data Expression = UnExp UOperator Expression | BiExp BOperator Expression Expression | C Int | V Char

instance Eq Expression where
	C x == C y = y == x
	V x == V y = y == x
	UnExp op x == UnExp op' x' = op == op' && x == x'
	BiExp Sum x y == BiExp Sum x' y' = (x == x' && y == y') || (x == y' && y == x')
	BiExp Diff x y == BiExp Diff x' y' = (x == x' && y == y') || (x == y' && y == x')
	BiExp Diff x y == BiExp Diff x' y' = x == x' && y == y'
	BiExp Div x y == BiExp Div x' y' = x == x' && y == y'
	BiExp Mul x y == BiExp Mul x' y' = (x == x' && y == y') || (x == y' && y == x')
	BiExp Pow x y == BiExp Pow x' y' = x == x' && y == y'
	

instance Show Expression where
	show (C x) = show x
	show (V x) = show x
	show (UnExp Sqrt e1) = "sqrt(" ++ show e1 ++ ")"
	show (UnExp Sin e1) = "sin(" ++ show e1 ++ ")"
	show (UnExp Cos e1) = "cos(" ++ show e1 ++ ")"
	show (UnExp Ln e1) = "ln(" ++ show e1 ++ ")"
	show (UnExp Exp e1) = "e^(" ++ show e1 ++ ")"
	show (BiExp Sum e1 e2) = show e1 ++ " + " ++ show e2
	show (BiExp Diff e1 e2) = show e1 ++ " - " ++ show e2
	show (BiExp Div e1 e2) = "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
	show (BiExp Mul e1 e2) = "(" ++ show e1 ++ ") * (" ++ show e2 ++ ")"
	show (BiExp Pow e1 e2) = "(" ++ show e1 ++ ")^(" ++ show e2 ++ ")"


(+:) a b = BiExp Sum a b
(-:) a b = BiExp Diff a b
(*:) a b = BiExp Mul a b
(/:) a b = BiExp Div a b
(^:) a b = BiExp Pow a b
sqrt' a = UnExp Sqrt a
sin' a = UnExp Sin a
cos' a = UnExp Cos a
ln' a = UnExp Ln a
e' a = UnExp Exp a


realfunc :: BOperator -> (Int -> Int -> Int)
realfunc Sum = (+)
realfunc Diff = (-)
realfunc Div = div
realfunc Mul = (*)
realfunc Pow = (^)


diff :: Expression -> Char -> Expression
diff (UnExp Sqrt e1) x = ((C 1) /: ((C 2) *: (e1 ^: (C 2)))) *: (diff e1 x)
diff (UnExp Sin e1) x = (cos' e1) *: (diff e1 x)
diff (UnExp Cos e1) x = ((C (-1)) *: (sin' e1)) *: (diff e1 x)
diff (UnExp Ln e1) x = ((C 1) /: e1) *: (diff e1 x)
diff (UnExp Exp e1) x = (e' e1) *: (diff e1 x)
diff (BiExp Pow e1 (C 0)) _ = C 0
diff (BiExp Pow e1 (C c)) x = let c' = C c in c' *: (e1 ^: (c' -: (C 1)))
diff (BiExp Pow e1 (V v)) x = let v' = V v in if v /= x then v' *: (e1 ^: (v' -: (C 1))) else diff (BiExp Pow e1 v') x
diff (BiExp Pow e1 e2) x = e1 *: (((diff e2 x) *: (ln' e1)) +: ((e2 /: e1) *: (diff e1 x)))
diff (BiExp Mul e1 e2) x = ((diff e1 x) *: e2) +: (e1 *: (diff e2 x))
diff (BiExp Div e1 e2) x = (((diff e1 x) *: e2) -: (e1 *: (diff e2 x))) /: (e2 ^: (C 2))
diff (BiExp Diff e1 e2) x = (diff e1 x) -: (diff e2 x)
diff (BiExp Sum e1 e2) x = (diff e1 x) +: (diff e2 x)
diff (V x') x = if x' == x then C 1 else C 0
diff (C _) _ = C 0


exact :: Expression -> Expression -> Bool
exact (C x) (C x') = x == x'
exact (V x) (V x') = x == x'
exact (BiExp op e1 e2) (BiExp op' e1' e2') = op == op && (exact e1 e1') && (exact e2 e2')
exact (UnExp op e) (UnExp op' e') = op == op && (exact e e')

simplify :: Expression -> Expression
simplify (BiExp op (C x) (C x')) = C (realfunc op x x')
simplify (BiExp Sum e (C 0)) = simplify e
simplify (BiExp Sum (C 0) e) = simplify e
simplify (BiExp Sum e e') = if e == e' then (C 2) *: simplify(e) else simplify(e) +: simplify(e')
simplify (BiExp Diff e (C 0)) = simplify(e)
simplify (BiExp Diff (C 0) e) = simplify(e)
simplify (BiExp Diff e e') = if e == e' then C 0 else simplify(e) -: simplify(e')
simplify (BiExp Mul (C 0) e) = C 0 
simplify (BiExp Mul e (C 0)) = C 0 
simplify (BiExp Mul (C 1) e) = simplify(e)
simplify (BiExp Mul e (C 1)) = simplify(e)
simplify (BiExp Mul e e') = if e == e' then simplify(e) ^: (C 2) else simplify(e) *: simplify(e')
simplify (BiExp Mul (C 1) e) = simplify e
simplify (BiExp Div (C 0) e) = C 0 
simplify (BiExp Div e (C 1)) = simplify e
simplify (BiExp Div e e') = if e == e' then C 1 else simplify(e) -: simplify(e')
simplify (BiExp Pow e (C 1)) = simplify e
simplify (BiExp Pow e (C 0)) = C 1
simplify e = let e' = simplify e in if exact e e' then e else simplify e'
