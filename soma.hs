data Number = Zero | S Number deriving Show

convertToInt :: Number -> Int
convertToInt Zero = 0
convertToInt (S x) = 1 + convertToInt x

convertToNumber :: Int -> Number
convertToNumber 0 = Zero
convertToNumber x = S (convertToNumber (x-1))

eq :: Number -> Number -> Bool
eq Zero Zero = True
eq Zero y = False
eq x Zero = False
eq (S x) (S y) = eq x y

soma :: Number -> Number -> Number
soma x Zero = x
soma x (S y) = S(soma x y)

mul :: Number -> Number -> Number
mul x Zero = Zero
mul x (S y) = soma x (mul x y)

pow :: Number -> Number -> Number
pow x Zero = S Zero
pow x (S y) = mul x (pow x y)

p :: Number -> Number
p Zero = Zero
p (S x1) = x1

r :: Number -> Number -> Number
r Zero y = Zero
r (S x) y
    | (eq (p y) (r x y)) = Zero
    | otherwise = S (r x y)

sub :: Number -> Number -> Number
sub x Zero = x
sub x (S y) = p (sub x y)
-- 8/2 = 4 => 4 * 2 = 8 => 2 + 2 + 2 + 2 = 8
d :: Number -> Number -> Number
d Zero x = Zero
d (S x) y
    | (eq (p y) (r x y)) = S (d x y)
    | otherwise = (d x y)

