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

-- Hanoi Stuff
-- After solving this exercise, playing this game dozens of times,
-- and knowing the optimal strategy, I still have no intuition to
-- how it works.
-- Yeah, I get you put stuff on one and up reversing the order
-- when you take it off, but my mind is blank.
-- Lucky me the solution is in the queston.
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 start goal scratch = []
hanoi n start goal scratch = (hanoi (n - 1) start scratch goal) ++ [(start, goal)] ++ (hanoi (n - 1) scratch goal start)
