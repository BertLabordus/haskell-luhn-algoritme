module Main where


removeLast :: [a] -> [a]
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x : removeLast xs

reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList xs = last xs : reverseList (removeLast xs)

multiplySecond :: Num a => [a] -> [a]
multiplySecond [] = []
multiplySecond [a] = [a]
multiplySecond (x:y:xs) = x : 2 * y : multiplySecond xs

modArray :: Integral a => [a] -> [a]
modArray xs = [doubleOdd x | x <- xs]

doubleOdd :: Integral a => a -> a
doubleOdd x = if x > 9
	then (x `mod` 10) + 1
	else x

sumArray :: (Num a) => [a] -> a
sumArray xs = sum' xs 0
	where 
		sum' [] c = c
		sum' (y:ys) c = sum' ys (c + y)

luhn :: Integral a => [a] -> Bool
luhn xs = sumArray( modArray( multiplySecond xs ) ) `mod` 10 == 0

luhnCheckDigit :: Integral a => [a] -> a
luhnCheckDigit xs = 10 - sumArray( modArray( multiplySecond xs ++ [0]) ) `mod` 10

main = print(luhn[7,9,9,2,7,3,9,8,7,1,3])