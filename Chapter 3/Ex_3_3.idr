import Data.Vect

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in (zipWith (::) x xsTrans)

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

dotProduct : Num a => Vect n a -> Vect n a -> a
dotProduct xs ys  = sum (zipWith (*) xs ys)

firstStep : Num a => Vect m a -> Vect p (Vect m a) -> Vect p a
firstStep x [] = []
firstStep x (y :: ys) = dotProduct x y :: firstStep x ys

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] ys = []
multMatrix (x :: xs) ys = let yTrans = transposeMat ys in (firstStep x yTrans) :: multMatrix xs ys
