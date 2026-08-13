import Prelude hiding (succ, pred, div)

data Peano = Zero | Succ Peano deriving (Show, Eq)

toPeano :: Integer -> Peano
toPeano 0 = Zero
toPeano x = Succ $ toPeano (x-1)

fromPeano :: Peano -> Integer
fromPeano Zero = 0
fromPeano (Succ x) = 1 + fromPeano x

succ :: Peano -> Peano
succ x = Succ x

pred :: Peano -> Peano
pred Zero = Zero
pred (Succ x) = x

add :: Peano -> Peano -> Peano
add Zero y = y
add (Succ x) y = Succ $ add x y

sub :: Peano -> Peano -> Peano
sub x Zero = x
sub x (Succ y) = pred $ sub x y

mult :: Peano -> Peano -> Peano
mult Zero y = Zero
mult (Succ x) y = add y (mult x y)

rest :: Peano -> Peano -> Peano
rest Zero y = Zero
rest (Succ x) y = if rest x y == pred y then Zero else succ $ rest x y

div :: Peano -> Peano -> Peano
div Zero y = Zero
div (Succ x) y = if rest x y == pred y then succ $ div x y else div x y
