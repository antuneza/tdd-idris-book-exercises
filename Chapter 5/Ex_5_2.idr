module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

guess: (target :Nat) -> (guesses : Nat) -> IO ()
guess target n = do
  putStr ("Try #" ++ show n ++ " - Guess a Number: ")
  Just input <- readNumber
    | Nothing => do
        printLn "Invalid input"
        guess target n
  case compare input target of
    LT => do
         putStrLn "Guess is too low"
         guess target (S n)
    GT => do
         putStrLn "Guess is too high"
         guess target (S n)
    EQ => do
         putStrLn "Guess is correct"
         pure ()

main: IO ()
main = do
  t <- time
  let rand = cast (1 + (mod t 100))
  guess rand 1

my_repl : (prompt : String) -> (onInput: String -> String) -> IO ()
my_repl prompt onInput = do
  putStr prompt
  input <- getLine
  putStrLn (onInput input)
  my_repl prompt onInput

my_replWith : (state : a) -> (prompt : String) -> (onInput: a -> String -> Maybe (String, a)) -> IO ()
my_replWith state prompt onInput = do
  putStr prompt
  input <- getLine
  let Just(output, newState) = onInput state input
      | Nothing => pure ()
  putStrLn output
  my_replWith newState prompt onInput
