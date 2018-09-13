import Data.Primitives.Views

every_other : Stream ty -> Stream ty
every_other (a :: b :: xs) = b :: every_other xs

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

countFrom: Integer -> InfList Integer
countFrom x = x :: countFrom (x+1)

getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

Functor InfList where
  map func (value :: xs) = func value :: map func xs

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

data Face = Heads | Tails

getFace : Int -> Face
getFace x with (divides x 2)
  getFace ((2 * div) + rem) | (DivBy prf) = if rem == 0 then Heads else Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count xs = take count (map getFace xs)

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx =
  let next = (approx + (number / approx)) / 2 in
    approx :: square_root_approx number next

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (val :: xs) = val
square_root_bound (S k) number bound (val :: xs) =
  if abs(val * val - number) < bound
    then val
    else square_root_bound k number bound xs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001 (square_root_approx number number)
