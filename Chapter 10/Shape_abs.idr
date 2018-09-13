module Shape_abs

export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

export
triangle: Double -> Double -> Shape
triangle = Triangle

export
rectangle: Double -> Double -> Shape
rectangle = Rectangle

export
circle : Double -> Shape
circle = Circle

public export
data ShapeView : Shape -> Type where
  STriangle : ShapeView (triangle b h)
  SRectangle : ShapeView (rectangle w h)
  SCircle : ShapeView (circle r)

export
shapeView: (s: Shape) -> ShapeView s
shapeView (Triangle base height) = STriangle
shapeView (Rectangle width height) = SRectangle
shapeView (Circle radius) = SCircle
