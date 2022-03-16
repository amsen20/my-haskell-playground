listEvens :: Int -> Int -> [Int]
listEvens x y  = [k | k <- [y, y-1 .. x], even k]

type Triple = (Int, Int, Int)
pythagoreanTriples :: Int -> [Triple]
pythagoreanTriples 1 = []
pythagoreanTriples n = [(a, b, n) | a <- [1 .. n], b <- [a .. n], a*a + b*b == n*n] ++ pythagoreanTriples (n-1)

addPairwise :: [Int] -> [Int] -> [Int]
addPairwise = zipWith (+)

subList :: [a] -> (Int, Int) -> [a]
subList xs (l, r) = [xs !! i | i <- [max 0 l .. min r (length xs - 1)]]

subList' :: [a] -> (Int, Int) -> [a]
subList' xs (l, r) = drop l (take (r+1) xs)

together :: [a] -> [(a, a)]
together xs = [(xs !! i, xs !! (i+1)) | i <- [0 .. (length xs - 2)]]

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (h : t) x = (x == h) || contains t x

nth :: Int -> [a] -> Maybe a
nth n xs
    | n < 0 = Nothing
    | n >= length xs = Nothing
    | otherwise = Just (xs !! n)

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (h : t)
    | h == x = remove x t
    | otherwise = h : remove x t

remove' :: Eq a => a -> [a] -> [a]
remove' x = filter (x /=)

substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y (h : t)
    | x == h = y : substitute x y t
    | otherwise = h : substitute x y t

addTime :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTime (a, b) (c, d)
    | b + d < 60 = (a + c, b + d)
    | otherwise = (a + c + 1, b + d - 60)

accTime :: [(Int, Int)] -> (Int, Int)
accTime [] = (0, 0)
accTime (h : t) = addTime h (accTime t)

accTime' :: [(Int, Int)] -> (Int, Int)
accTime' = foldr addTime (0, 0)

addDashes :: [String] -> [String]
addDashes = map ( ++ "/")

swapPairs :: [(a, b)] -> [(b, a)]
swapPairs = map (\(x, y) -> (y, x))

swapPairs' :: [(a, b)] -> [(b, a)]
swapPairs' xs = [(y, x) | (x, y) <- xs]

applyEach :: [(a -> b, a)] -> [b]
applyEach = map (\(f, x) -> f x)

theSame :: String -> Bool
theSame str = foldr (&&) True (map (\c -> c == (str !! 0)) str)

twice :: a -> (a -> a) -> a
twice x f = (f . f) x

maxfRec :: (Int -> Int) -> Int -> Int
maxfRec f 0 = f 0
maxfRec f n = max (f n) (maxfRec f (n-1))

oneZero :: (Int -> Int) -> Int -> Bool
oneZero f 0 = f 0 == 0
oneZero f n = (f n == 0) || oneZero f (n-1)

iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (h : t) = h + (mySum t)

mySum' :: Num a => [a] -> a
mySum' = foldr (+) 0

merge :: (a, b) -> ([a], [b]) -> ([a], [b])
merge (x, y) (xs, ys) = (x : xs, y : ys)

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = foldr merge ([], [])

natInd :: a -> (Int -> a -> a) -> Int -> a
natInd base step 0 = base
natInd base step n = step n (natInd base step (n-1))

fact :: Int -> Int
fact = natInd 1 (\n -> (\prev -> n * prev))
