fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = if n > 0 then (n `mod` 10) : toDigitsRev(n `div` 10) else []

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev(n))

