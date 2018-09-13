import DataStore
import Shape_abs

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues input with (storeView input)
  getValues input | SNil = []
  getValues (addToStore (key, value) store') | (SAdd rec) = value :: getValues store' | rec

--------------------------------------------------------------------------------
area: Shape -> Double
area s with (shapeView s)
  area (triangle b h) | STriangle = 0.5 * b * h
  area (rectangle w h) | SRectangle = w * h
  area (circle r) | SCircle = pi * r * r
