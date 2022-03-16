import Data.Tuple
import Control.Monad (liftM, ap)

helloWorld :: IO ()
helloWorld = do
    putStr "Hello world!\n"

helloWorld' :: IO ()
helloWorld' = putStr "Hello world!\n"

youSaid :: IO ()
youSaid = do
    msg <- getLine
    putStr ("You said: " ++ msg)

youSaid' :: IO ()
youSaid' = getLine >>= \msg -> putStr ("You said: " ++ msg)

ioReverse :: IO ()
ioReverse = do
    putStrLn "Give me a string!"
    str <- getLine
    if null str then
        do
            putStrLn "Next time enter a non-empty string!"
            putStrLn "Capiche?!"
        else
            putStrLn ("The reverse of " ++ str ++ " is " ++ (reverse str) ++ ".")

ioReverse' :: IO ()
ioReverse' = do
    _ <- putStrLn "Give me a string!"
    str <- getLine
    _ <- if null str then
        do
            _ <- putStrLn "Next time enter a non-empty string!"
            putStrLn "Capiche?!"
        else
            putStrLn ("The reverse of " ++ str ++ " is " ++ (reverse str) ++ ".")
    return ()

ioReverse'' :: IO ()
ioReverse'' =
    putStrLn "Give me a string!" >>=
    \_ -> getLine >>=
    \str -> if null str then
                putStrLn "Next time enter a non-empty string!" >>=
                \_ -> putStrLn "Capiche?!"
            else
                putStrLn ("The reverse of " ++ str ++ " is " ++ (reverse str) ++ ".")

ioReverse''' :: IO ()
ioReverse''' = putStrLn "Give me a string!" >>=
    \_ -> getLine >>=
    \str -> (if null str then
                putStrLn "Next time enter a non-empty string!" >>=
                \_ -> putStrLn "Capiche?!"
            else
                putStrLn ("The reverse of " ++ str ++ " is " ++ (reverse (str)) ++ ".")) >>=
    \_ -> getLine >>=
    \str2 -> putStrLn ("The reverse of " ++ str2 ++ " is " ++ (reverse (str2)) ++ ".")


type ID = Int
type Name = String
type Country = String
type Town = String

nameTable :: [(ID, Name)]
nameTable = [
    (1, "Aybeesky"),
    (2, "Beeseesky"),
    (3, "Seedeesky"),
    (4, "Ayefsky"),
    (5, "Deebeesky")
    ]

btownTable :: [(ID, Town)]
btownTable = [
    (1, "Teetown"),
    (2, "Eston"),
    (3, "Arby"),
    (5, "Esfield")
    ]  -- 4th is missing

townCountryTable :: [(Town, Country)]
townCountryTable = [
    ("Teetown", "Teeland"),
    ("Eston", "Exland"),
    ("Arby", "Teeland")
    ]  -- "Esfield" is missing

getID :: Name -> Maybe ID
getID name = search name nameTable where
    search name [] = Nothing
    search name (h : t) = if snd h == name then Just (fst h) else search name t

getID' :: Name -> Maybe ID
getID' name = lookup name $ map swap nameTable

getCountry :: Town -> Maybe Country
getCountry town = lookup town townCountryTable

getBTown :: ID -> Maybe Town
getBTown id = lookup id btownTable

getBCountry :: Name -> Maybe Country
getBCountry name = do
    id <- getID name
    town <- getBTown id
    getCountry town

getBCountry' :: Name -> Maybe Country
getBCountry' name = getID name >>=
    getBTown >>=
    getCountry


data State s a = State (s -> (s,a))

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure = return
    (<*>) = ap

instance Monad (State s) where
    -- aState :: s -> (s, a)
    -- f :: a -> State s b
    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) (State aState) f =
      State $ (\s0 ->
        let (s1, a) = aState s0 in
        let State g = f a  -- g :: s -> (s, b)
        in  g s1)  -- s -> (s, b) 
    -- return :: a -> (State s) a
    return x = State $ \s -> (s,x)

setState :: s -> State s ()
setState x = State $ \s -> (x, ())

getState :: State s s
getState =  State (\s -> (s,s))

runState :: s -> State s a -> (s, a)
runState initState (State step) = step initState

type Sum = Float
type Item = (String, Float)

getItem :: ID -> Item
getItem 1 = ("Fries", 0.50)
getItem 2 = ("Sandwich", 3.50)
getItem 3 = ("Schnitzel", 8.50)
getItem _ = ("khiar", 0.0)

addItem :: ID -> State Sum Item
addItem id = let (name, price) = getItem id in
    State $
        \s -> (s + price, (name, price))

addItem' :: ID -> State Sum Item
addItem' id = do
    curr <- getState
    setState (curr + snd item)
    return item
        where item = getItem id

addItems :: State Sum Item
addItems = do
    addItem 1
    addItem 2
    addItem 3

addItems' :: State Sum Item
addItems' = addItem' 1 >> (addItem' 2 >> addItem' 3)

type Register = [Item]

registerItem :: ID -> State Register Item
registerItem id = do
    curr <- getState
    setState (curr ++ [item])
    return item
        where item = getItem id

registerItems :: State Register Int
registerItems = do
    registerItem 1
    registerItem 2
    registerItem 3
    registerItem 4
    curr <- getState
    return (length curr)

regPrices :: State Register Sum
regPrices = do
    curr <- getState
    return $ sum $ map snd curr

regPrices' :: State Register Sum
regPrices' = sum <$> map snd <$> getState

type Stack = [Int]

pop :: State Stack (Maybe Int)
pop = do
    curr <- getState
    if not (null curr) then
        do
            let (x : xs) = curr in setState xs
            let (x : xs) = curr in return (Just x)
        else
            return Nothing

pop' :: State Stack (Maybe Int)
pop' = State tryPop where
    tryPop [] = ([], Nothing)
    tryPop (x:xs) = (xs, Just x)

push :: Int -> State Stack ()
push n = do
    curr <- getState
    setState (n:curr)
    return ()

push' :: Int -> State Stack ()
push' n = State $ \xs -> (n:xs, ())

stackTest :: State Stack (Maybe Int)
stackTest = do 
    push 3
    push 7
    push 9
    a <- pop
    pop
    push 2
    return a

add :: State Stack ()
add = do -- State stack context
    maybeA <- pop
    maybeB <- pop
    let maybeSum = do -- Maybe context
        a <- maybeA
        b <- maybeB
        return (a + b) in 
        case maybeSum of
            Nothing -> return ()
            Just x -> push x

stackTest' :: State Stack (Maybe Int)
stackTest' = do 
    push 3
    push 7
    push 9
    add
    a <- pop
    return a
