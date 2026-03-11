-- Credit Card Number Validation
toDigits :: Int -> [Int]
toDigits x
    | x < 10 = [x]
    | otherwise = toDigits (div x 10) ++ [mod x 10]

toDigitsRev :: Int -> [Int]
toDigitsRev x
    | x < 10 = [x]
    | otherwise = (mod x 10) : toDigitsRev (div x 10)

doubleIt :: [Int] -> [Int]
doubleItHelper :: [Int] -> [Int]

doubleIt xs = reverse (doubleItHelper (reverse xs))

doubleItHelper [] = []
doubleItHelper (x:[]) = [x]
doubleItHelper (x:y:xs) = [x] ++ [2 * y] ++ doubleItHelper xs

sumDigits :: [Int] -> Int
sumDigits xs = sum (map sum (map toDigits xs))

validate :: Int -> Bool
validate x = mod (sumDigits (doubleIt (toDigits x))) 10 == 0

-- The Towers of Hanoi
-- String is Peg
type Move = (String, String)
hanoi :: Int -> String -> String -> String -> [Move]

hanoi 0 _ _ _ = []
hanoi n from via to = hanoi (n-1) from to via ++ [(from, to)] ++ hanoi (n-1) via from to
