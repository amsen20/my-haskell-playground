double :: Int -> Int
double x = x * 2

ratio :: Float -> Float -> Float 
ratio x y = (x - y) / (x + y)

hypotenuse :: Double -> Double -> Double
hypotenuse x y = sqrt (x*x + y*y)

xIntercept :: Double -> Double -> Double
xIntercept m c = -c/m

threeDiff :: Integer -> Integer -> Integer -> Bool
threeDiff x y z = (x /= y) && (x /= z) && (y /= z)

averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromInteger (x + y + z) / 3

arithmeticSum :: Integer -> Integer -> Integer -> Integer
arithmeticSum a n d = n * (2*a + (n-1)*d) `div` 2

inrange1 :: Integer -> Integer -> Integer -> Bool
inrange1 x a b = min a b <= x && x <= max a b

orExclusive :: Bool -> Bool -> Bool
orExclusive x y = (not x && y) || (x && not y)

implies :: Bool -> Bool -> Bool 
implies a b = not a || b

hundreds :: Integer -> Integer
hundreds a = (a `div` 100) `mod` 10

middle :: String -> Char
middle str = str !! (length str `div` 2)

getNth :: Int -> Int
getNth 1 = 0
getNth n = 1 + getNth (n - 1)

getNthOdd :: Int -> Int
getNthOdd 1 = 1
getNthOdd n = 2 + getNthOdd (n - 1)

sumToN :: Int -> Int 
sumToN 1 = 0
sumToN n = n-1 + sumToN (n - 1)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

sumFact :: Int -> Int
sumFact 0 = fact 0
sumFact n = fact n + sumFact (n - 1)

arithmeticSeries :: Int -> Int -> Int -> Int
arithmeticSeries a 1 d = a
arithmeticSeries a n d = if n <= 0 then a else 
    d + arithmeticSeries a (n - 1) d

getNthOdd1 :: Int -> Int
getNthOdd1 n = arithmeticSeries 1 n 2

arithmeticSumRec :: Int -> Int -> Int -> Int
arithmeticSumRec a 1 d = a
arithmeticSumRec a n d = arithmeticSeries a n d + arithmeticSumRec a (n - 1) d

multRec :: Int -> Int -> Int
multRec m 0 = 0
multRec m n = multRec m (n-1) + m

rangeProduct :: Int -> Int -> Int
rangeProduct m n = if m <= n then m * rangeProduct (m+1) n else 1

intSqrt' :: Int -> Int -> Int 
intSqrt' n i = if i*i <= n then i else intSqrt' n (i-1)

intSqrt :: Int -> Int
intSqrt n = intSqrt' n n

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

step :: (Int, Int) -> (Int , Int)
step (x, y) = (y, x + y)

fiboTwo :: Int -> (Int, Int)
fiboTwo 0 = (0, 1)
fiboTwo n = step (fiboTwo (n-1))

superFibo :: Int -> Int
superFibo n = fst (fiboTwo n)
