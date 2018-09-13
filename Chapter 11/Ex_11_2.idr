import Data.Primitives.Views
import System

%default total

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run: Fuel -> InfIO -> IO ()
run Dry _ = putStrLn "Out of fuel"
run (More fuel) (Do x f) = do res <- x
                              run fuel (f res)
partial
forever : Fuel
forever = More forever

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do
  putStr prompt
  input <- getLine
  putStr (action input)
  totalREPL prompt action
