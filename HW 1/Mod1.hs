-- Converts an Integer to a list of digits, but in the reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n < 10 = [n]
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- Converts an Integer to a list of digits
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Doubles every other elements in a list of Integers, moving left
doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft []       = []
doubleEveryOtherLeft (a:[])   = a : []
doubleEveryOtherLeft (a:b:bs) = a : b * 2 : doubleEveryOtherLeft(bs)

--Doubles every other element in a list of Integers, starting from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse(doubleEveryOtherLeft(reverse(l)))

-- Sums a list of Integers
sumDigits :: [Integer] -> Integer
sumDigits (n:[]) = n
sumDigits (n:xn) = n + sumDigits(xn)

-- Calculates the remainder of the integer when divided by 10
remainderDigits :: Integer -> Integer
remainderDigits n = (sumDigits(doubleEveryOther(toDigits n)) `mod` 10)

validate :: Integer -> Bool
validate n = remainderDigits(n) == 0