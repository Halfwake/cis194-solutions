-- Credit Card Stuff

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- You can't even run a search for standard library solutions
-- to this problem because the internet is filled with people
-- asking about this specific function.
-- Enjoy my awful handmade solution!
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ recurseNoDouble $ reverse xs
  where recurseWithDouble [] = []
        recurseWithDouble (x:xs) = (x * 2) : (recurseNoDouble xs)
        recurseNoDouble [] = []
        recurseNoDouble (x:xs) = x : (recurseWithDouble xs)

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs
  where sum = foldl (+) 0

validate:: Integer -> Bool
validate n = (checksum `mod` 10) == 0
  where checksum = sumDigits $ doubleEveryOther $ toDigits n
