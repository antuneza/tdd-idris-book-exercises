data Shape = Triangle Double Double
  | Rectangle Double Double
  | Circle Double

Eq Shape where
  (==) (Triangle a b) (Triangle c d) = a == b && c == d
  (==) (Rectangle a b) (Rectangle c d) = a == b && c == d
  (==) (Circle r) (Circle r') = r == r'
  (==) _ _ = False

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

Ord Shape where
  compare a b = compare (area a) (area b)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
