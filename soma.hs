data Number = Zero | S Number deriving Show

soma :: Number -> Number -> Number
soma x Zero = x
soma x (S y) = S(soma x y)

mul :: Number -> Number -> Number
mul x Zero = Zero
mul x (S y) = soma x (mul x y)

pow :: Number -> Number -> Number
pow x Zero = S Zero
pow x (S y) = mul x (pow x y)

