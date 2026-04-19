-- implementação da função mod(x, 2) = { 1, se x é ímpar; 0, c. c. }

data PeaNumber = Zero | Succ PeaNumber deriving (Show, Eq)

one = Succ Zero
two = Succ one

less :: PeaNumber -> PeaNumber -> PeaNumber
less x Zero = Zero
less Zero y = one
less (Succ x) (Succ y) = less x y

peapred :: PeaNumber -> PeaNumber
peapred Zero = Zero
peapred (Succ x) = x

sub2 = peapred . peapred

mod2 :: PeaNumber -> PeaNumber
mod2 x
    | (less x two == one) = x
    | otherwise = mod2 (sub2 x) 

convertToInt :: PeaNumber -> Int
convertToInt Zero = 0
convertToInt (Succ x) = 1 + convertToInt x

convertToNumber :: Int -> PeaNumber
convertToNumber 0 = Zero
convertToNumber x = Succ (convertToNumber (x-1))

mod2Int = convertToInt . mod2 . convertToNumber
