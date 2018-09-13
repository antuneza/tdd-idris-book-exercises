data Elem : a -> List a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

--------------------------------------------------------------------------------

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf: Last xs value) -> Last (x :: xs) value

last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)

emptyListNoLast : Last [] value -> Void
emptyListNoLast _ impossible

noLastInListOfOne : (contra : (x = value) -> Void) -> Last [x] value -> Void
noLastInListOfOne contra LastOne = contra Refl
noLastInListOfOne _ (LastCons _) impossible

noLastInList : (contra : Last (x :: xs) value -> Void) -> Last (y :: (x :: xs)) value -> Void
noLastInList contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No emptyListNoLast
isLast (x :: []) value = case decEq x value of
  Yes Refl => Yes LastOne
  No contra => No (noLastInListOfOne contra)
isLast (y :: x :: xs) value = case isLast (x :: xs) value of
  Yes prf => Yes (LastCons prf)
  No contra=> No (noLastInList contra)
