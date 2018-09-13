import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackCmd () height (S height)
  Pop : StackCmd Integer (S height) height
  Top : StackCmd Integer (S height) (S height)
  GetStr : StackCmd String h h
  PutStr : String -> StackCmd () h h
  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd a h1 h2 -> (a -> StackCmd b h2 h3) -> StackCmd b h1 h3

runStack : (stk : Vect inHeight Integer) -> StackCmd ty inHeight outHeight -> IO (ty, Vect outHeight Integer)
runStack stk (Push x) = pure ((),  x :: stk)
runStack (x :: xs) Pop = pure (x, xs)
runStack (x :: xs) Top = pure (x, x :: xs)
runStack stk (Pure x) = pure (x, stk)
runStack stk (cmd >>= next) = do
   (cmdRes, stk') <- runStack stk cmd
   runStack stk' (next cmdRes)
runStack stk GetStr = do
  input <- getLine
  pure (input, stk)
runStack stk (PutStr str) = do
  putStr str
  pure ((), stk)

data StackIO : Nat -> Type where
  Do : StackCmd a h1 h2 -> (a -> Inf (StackIO h2)) -> StackIO h1

namespace StackDo
  (>>=) : StackCmd a h1 h2 -> (a -> Inf (StackIO h2)) -> StackIO h1
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run (More fuel) stk (Do c f) = do
  (res, stk') <- runStack stk c
  run fuel stk' (f res)
run Dry stk p = pure ()

data StkInput = Number Integer | Add | Sub | Mult | Neg | Discard | Duplicate

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput "subtract" = Just Sub
strToInput "multiply" = Just Mult
strToInput "negate" = Just Neg
strToInput "discard" = Just Discard
strToInput "duplicate" = Just Duplicate
strToInput x = if all isDigit (unpack x)
                then Just (Number (cast x))
                else Nothing

doOp : (Integer -> Integer -> Integer) -> StackCmd () (S (S height)) (S height)
doOp f = do
  a <- Pop
  b <- Pop
  Push (f b a)

mutual
  tryOp : (Integer -> Integer -> Integer) -> StackIO height
  tryOp f {height = (S (S h))} = do
    doOp f
    result <- Top
    PutStr (show result ++ "\n")
    stackCalc
  tryOp f = do
    PutStr ("Fewer than two items in the stack\n")
    stackCalc

  tryNeg : StackIO height
  tryNeg {height = (S h)} = do
      n <- Pop
      Push (-n)
      result <- Top
      PutStr (show result ++ "\n")
      stackCalc
  tryNeg = do
      PutStr ("No elements in the stack\n")
      stackCalc

  tryPop : StackIO height
  tryPop {height = (S h)} = do
    n <- Pop
    PutStr ("Discarded " ++ (show n) ++ "\n")
    stackCalc
  tryPop = do
    PutStr ("No elements in the stack\n")
    stackCalc

  tryDup : StackIO height
  tryDup {height = (S h)} = do
    n <- Top
    Push n
    PutStr ("Duplicated " ++ (show n) ++ "\n")
    stackCalc
  tryDup = do
    PutStr ("No elements in the stack\n")
    stackCalc

  stackCalc  : StackIO height
  stackCalc = do
    PutStr "> "
    input <- GetStr
    case strToInput input of
      Nothing => do
        PutStr "Invalid input\n"
        stackCalc
      Just (Number x) => do
        Push x
        stackCalc
      Just Add => do
        tryOp (+)
      Just Sub => do
        tryOp (-)
      Just Mult => do
        tryOp (*)
      Just Neg => do
        tryNeg
      Just Discard => do
        tryPop
      Just Duplicate => do
        tryDup


main : IO()
main = run forever [] stackCalc
