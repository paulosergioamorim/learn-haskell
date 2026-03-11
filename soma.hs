data Number = Zero | S Number deriving Show

convertToInt :: Number -> Int
convertToInt Zero = 0
convertToInt (S x) = 1 + convertToInt x

convertToNumber :: Int -> Number
convertToNumber 0 = Zero
convertToNumber x = S (convertToNumber (x-1))

soma :: Number -> Number -> Number
soma x Zero = x
soma x (S y) = S(soma x y)

mul :: Number -> Number -> Number
mul x Zero = Zero
mul x (S y) = soma x (mul x y)

pow :: Number -> Number -> Number
pow x Zero = S Zero
pow x (S y) = mul x (pow x y)

