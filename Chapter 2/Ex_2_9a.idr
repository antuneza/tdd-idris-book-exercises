module Main

palindrome : Nat -> String -> Bool
palindrome n str = let lowstr = toLower str in
                       length lowstr > n && lowstr == (reverse lowstr)

main : IO ()
main = do
  putStr "Enter a string: "
  input <- getLine
  let isPalindrome = palindrome 10 input
  printLn (show isPalindrome)
