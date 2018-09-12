import Data.Vect

readToBlank : IO (List String)
readToBlank = do putStrLn "Enter strings (blank line to end): "
                 input <- getLine
                 if input == ""
                   then pure []
                   else do
                     xs <- readToBlank
                     pure (input :: xs)

readAndSave : IO()
readAndSave = do
                consoleInput <- readToBlank
                putStrLn "Enter filename to create: "
                fileName <- getLine
                Right _ <- writeFile fileName (unlines consoleInput)
                 | Left err => printLn err
                printLn ("Saved to file " ++ fileName)

readVectFHandle : (h : File) -> IO (n ** Vect n String)
readVectFHandle h = do
  eof <- fEOF h
  if eof then pure (_ ** [])
  else do
    Right x <- fGetLine h
    | Left err => pure (_ ** [])
    (_ ** xs) <- readVectFHandle h
    pure (_ ** x :: xs)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right h <- openFile filename Read
  | Left err => pure (_ ** [])
  output <- readVectFHandle h
  _ <- closeFile h
  pure output
