-- The Luhn algorithm or Luhn formula, also known as the "modulus 10" or "mod 10" 
-- algorithm, is a simple checksum formula used to validate a variety of identification 
-- numbers, such as credit card numbers, IMEI numbers, National Provider Identifier 
-- numbers in US and Canadian Social Insurance Numbers. It was created by IBM scientist 
-- Hans Peter Luhn and described in U.S. Patent No. 2,950,048, filed on January 6, 1954, 
-- and granted on August 23, 1960.

module Main where

-- Remove last element in list
removeLast :: [a] -> [a]
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x : removeLast xs

-- Reverse list 
reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList xs = last xs : reverseList (removeLast xs)

-- Multiply every second element in list
multiplySecond :: Num a => [a] -> [a]
multiplySecond [] = []
multiplySecond [a] = [a]
multiplySecond (x:y:xs) = x : 2 * y : multiplySecond xs

-- Does mod 10 + 1 if input is bigger than 9
doubleOdd :: Integral a => a -> a
doubleOdd x = if x > 9
	then (x `mod` 10) + 1
	else x

-- Loop through list and execute doubleOdd on every element
modArray :: Integral a => [a] -> [a]
modArray xs = [doubleOdd x | x <- xs]


-- Sum of elements in list
sumArray :: (Num a) => [a] -> a
sumArray xs = sum' xs 0
	where 
		sum' [] c = c
		sum' (y:ys) c = sum' ys (c + y)

-- Apply luhn algoritme on list, returns true if correct number
luhn :: Integral a => [a] -> Bool
luhn xs = sumArray( modArray( multiplySecond xs ) ) `mod` 10 == 0

-- Calculate checkdigit of creditcardnumber
luhnCheckDigit :: Integral a => [a] -> a
luhnCheckDigit xs = 10 - sumArray( modArray( multiplySecond xs ++ [0]) ) `mod` 10
