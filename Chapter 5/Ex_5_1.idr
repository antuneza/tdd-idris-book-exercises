printLongerDo : IO ()
printLongerDo = do putStr "Input First String: "
                   inputA <- getLine
                   putStr "Input Second String: "
                   inputB <- getLine
                   let maxlen = max (length inputA) (length inputB)
                   putStrLn (show maxlen)

printLonger : IO()
printLonger = putStr "Input First String: " >>= \_ =>
              getLine >>= \inputA =>
              putStr "Input Second String: " >>= \_ =>
              getLine >>= \inputB =>
              let maxlen = max (length inputA) (length inputB) in
              putStrLn (show maxlen)
