
import System
import Data.Primitives.Views

%default total

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String
  ReadFile : String -> Command (Either FileError String)
  WriteFile : String -> String -> Command (Either FileError ())
  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile filepath) = readFile filepath
runCommand (WriteFile filepath contents) = writeFile filepath contents
runCommand (Pure val) = pure val
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do: Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run: Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry _ = pure Nothing
run (More fuel) (Quit value) = pure (Just value)
run (More fuel) (Do c f) = do
  res <- runCommand c
  run fuel (f res)

partial
forever: Fuel
forever = More forever

data Input = CatCmd String | CopyCmd String String | QuitCmd | InvalidCmd

parseCommand : (cmd : String) -> (args : String) -> Input
parseCommand "cat" fileName = CatCmd fileName
parseCommand "copy" args = let (source, dest) = span (/= ' ') args in CopyCmd source (ltrim dest)
parseCommand "exit" "" = QuitCmd
parseCommand _ _ = InvalidCmd

parse: (input : String) -> Input
parse input = case span (/= ' ') input of
                (cmd, args) => parseCommand cmd (ltrim args)

readInput : (prompt : String) -> Command Input
readInput prompt = do
  PutStr prompt
  input <- GetLine
  Pure (parse input)

shell : ConsoleIO ()
shell = do
  input <- readInput ("Enter a valid command: ")
  case input of
        CatCmd file => do
            Right content <- ReadFile file
            | Left err => do
              PutStr "Error reading file\n"
              shell
            PutStr content
            shell
        (CopyCmd source dest) => do
            Right content <- ReadFile source
            | Left err => do
              PutStr "Error reading file\n"
              shell
            Right _ <- WriteFile dest content
            | Left err => do
              PutStr "Error writing file\n"
              shell
            shell
        QuitCmd => Quit ()
        InvalidCmd => shell

partial
main : IO ()
main = do
  Just _ <- run forever shell
  | Nothing => putStrLn "Ran out of fuel"
  pure ()
