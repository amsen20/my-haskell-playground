data Season = Spring | Summer | Autumn | Winter
    deriving (Eq, Show, Enum)

theSeasons :: [Season]
theSeasons = [Spring, Summer, Autumn, Winter]

theSeasons' :: [Season]
theSeasons' = enumFrom Spring

seasonsFrom :: Season -> [Season]
seasonsFrom = enumFrom

mapSeasonsFrom :: [Season] -> [[Season]]
mapSeasonsFrom = map seasonsFrom

data Month = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec
    deriving (Eq, Ord, Show, Enum)

monthNumber :: Month -> Int
monthNumber month = fromEnum month + 1

monthFromTo :: Month -> Month -> [Month]
monthFromTo l r = [l .. r]

monthToSeason :: Month -> Season
monthToSeason month = toEnum $ ((fromEnum month `div` 3) + 3) `mod` 4

data MyBoolean = MyFalse | MyTrue deriving (Show)

boolToMyBoolean :: Bool -> MyBoolean
boolToMyBoolean True = MyTrue
boolToMyBoolean False = MyFalse

myBooleanToBool :: MyBoolean -> Bool
myBooleanToBool MyTrue = True
myBooleanToBool MyFalse = False

(&:&) :: MyBoolean -> MyBoolean -> MyBoolean
MyTrue &:& MyTrue = MyTrue
_ &:& _ = MyFalse

(|:|) :: MyBoolean -> MyBoolean -> MyBoolean
MyFalse |:| MyFalse = MyFalse
_ |:| _ = MyTrue

myAnd :: [MyBoolean] -> MyBoolean
myAnd [] = MyTrue
myAnd (MyTrue : t) = myAnd t
myAnd (MyFalse : _) = MyFalse

myAnd' :: [MyBoolean] -> MyBoolean
myAnd' = foldr (&:&) MyTrue

myAnd'' :: [MyBoolean] -> MyBoolean
myAnd'' = boolToMyBoolean . and . map myBooleanToBool

myOr :: [MyBoolean] -> MyBoolean
myOr = foldr (|:|) MyFalse

data Bit = O | I
    deriving (Enum, Show)

bitsToInt :: [Bit] -> Int
bitsToInt = construct 0
    where
        construct n [] = n
        construct n (h : t) = construct (2*n + fromEnum h) t

bitsToInt' :: [Bit] -> Int
bitsToInt' = foldr (\b n -> n*2 + fromEnum b) 0

data Number = Exact Int | Approx Float

rounded :: Number -> Int
rounded (Exact x) = x
rounded (Approx x) = round x

data Age = Years Int
    deriving Show

data Name = Name String String
    deriving Show

data Person = Person Name Age
    deriving Show

firstName :: Person -> String
firstName (Person (Name first _) _) = first

howOld :: Person -> Int
howOld (Person _ (Years x)) = x

addAges :: Person -> Person -> Int
addAges p1 p2 = howOld p1 + howOld p2

addAges' :: Person -> Person -> Int
addAges' (Person _ (Years x1)) (Person _ (Years x2)) = x1 + x2

data Expr = Lit Int |
    Add Expr Expr |
    Sub Expr Expr |
    Mul Expr Expr |
    Div Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

size :: Expr -> Int
size (Lit _) = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Sub e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (Div e1 e2) = 1 + size e1 + size e2

data Expr' = Lit' Int | OpExp Op Expr' Expr'

data Op = Add' | Sub' | Mul' | Div'
    deriving Eq

eval' :: Expr' -> Int
eval' (Lit' n) = n
eval' (OpExp Add' e1 e2) = eval' e1 + eval' e2
eval' (OpExp Sub' e1 e2) = eval' e1 - eval' e2
eval' (OpExp Mul' e1 e2) = eval' e1 * eval' e2
eval' (OpExp Div' e1 e2) = eval' e1 `div` eval' e2

data Pair a = Pair a a deriving Show

swapPair :: Pair a -> Pair a
swapPair (Pair x y) = Pair y x

eqPair :: Eq a => Pair a -> Bool
eqPair (Pair x y) = x == y

data List a = EmptyList | Cons a (List a)
    deriving (Eq,Ord,Show,Read)

isEmpty :: List a -> Bool
isEmpty EmptyList = True
isEmpty _ = False

lengthOfList :: List a -> Int
lengthOfList EmptyList = 0
lengthOfList (Cons _ t) = 1 + lengthOfList t

data Tree a = Empty |
    Leaf a |
    Node a (Tree a) (Tree a)
    deriving Show

howMany :: Tree a -> Int
howMany Empty = 0
howMany (Leaf _) = 1
howMany (Node _ t1 t2) = 1 + howMany t1 + howMany t2

depth :: Tree a -> Int
depth Empty = 0
depth (Leaf _) = 1
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

reflect :: Tree a -> Tree a
reflect Empty = Empty
reflect (Leaf n) = Leaf n
reflect (Node n t1 t2) = Node n (reflect t2) (reflect t1)

post :: Tree a -> [a]
post Empty = []
post (Leaf n) = [n]
post (Node n t1 t2) = post t1 ++ post t2 ++ [n]

pre :: Tree a -> [a]
pre Empty = []
pre (Leaf n) = [n]
pre (Node n t1 t2) = [n] ++ pre t1 ++ pre t2

myTraverse :: Tree a -> [a]
myTraverse Empty  =  []
myTraverse (Leaf a) = [a]
myTraverse (Node a tree1 tree2) = myTraverse tree1 ++ [a] ++ myTraverse tree2

normalise :: Tree a -> Tree a
normalise (Node n Empty Empty) = Leaf n
normalise (Node n t1 t2) = Node n (normalise t1) (normalise t2) 
normalise tree = tree

myTree :: Tree Int
myTree = normalise (Node 1 (Node 2 (Node 3 Empty Empty) (Leaf 4)) (Leaf 5))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Leaf n) = Leaf (f n)
mapTree f (Node n t1 t2) = Node (f n) (mapTree f t1) (mapTree f t2)

data GenTree a = GenTree a [GenTree a]
    deriving Show

leaves :: GenTree a -> Int
leaves (GenTree _ []) = 1
leaves (GenTree _ childs) = sum (map leaves childs)
