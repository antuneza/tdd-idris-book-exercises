module Main

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

main : IO ()
main = do
  putStr "Enter a string: "
  input <- getLine
  let (ws, cs) = counts input
  printLn ("Words: " ++ show ws ++ " / Characters: " ++ show cs)
