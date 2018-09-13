import Data.List.Views
import Data.Vect.Views
import Data.Nat.Views
import Data.Vect

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (zs ++ [x]) ys | (Snoc recxs) with (snocList ys)
    equalSuffix (zs ++ [x]) [] | (Snoc recxs) | Empty = []
    equalSuffix (zs ++ [x]) (ws ++ [y]) | (Snoc recxs) | (Snoc recys)
      = if x == y
        then (equalSuffix zs ws | recxs | recsys) ++ [x]
        else []

mergeSort: Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (ys ++ zs) | (SplitRecPair lrec rrec)
   = merge (mergeSort ys | lrec) (mergeSort zs | rrec)

{- Error in Idris 1.30. Cannot fully evaluate, need exec -}
toBinary: (n: Nat) -> String
toBinary n with (halfRec n)
  toBinary Z | HalfRecZ = ""
  toBinary (x + x) | (HalfRecEven rec) = (toBinary x | rec) ++ "0"
  toBinary (S (x + x)) | (HalfRecOdd rec) = (toBinary x | rec) ++ "1"

palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec)
    = if x == y
      then palindrome ys | rec
      else False
