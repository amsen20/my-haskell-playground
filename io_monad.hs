inc :: Int -> IO Int
inc x = return (x+1)

printInt :: Int -> IO ()
printInt x = putStrLn (show x)

incIO :: Int -> IO Int
incIO x = do 
    y <- inc x
    _ <- printInt y
    return y