||| 1. enumeration type example

data Direction = North | East | South | West

turnClockwise: Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North


||| 2. Union type example

||| Represent shapes
data Shape = ||| A triangle, with its base length and height
            Triangle Double Double
          | ||| A rectangle, with its length and height
            Rectangle Double Double
          | ||| A circle, with its radius
            Circle Double

area: Shape -> Double
area (Triangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle x) = x * x

||| Another way to define type

data Shape2 : Type where
  Triangle2 : Double -> Double -> Shape2
  Rectangle2 : Double -> Double -> Shape2
  Circle2 : Double -> Shape2


||| 3. Recursive types

data Nat2 = Z2 | S2 Nat2

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture: Picture
testPicture = Combine (Translate 5 5 rectangle)
                (Combine (Translate 35 5 circle) (Translate 15 25 triangle))

pictureArea : Picture -> Double
pictureArea (Primitive x) = area x
pictureArea (Combine x y) = pictureArea x + pictureArea y
pictureArea (Rotate x y) = pictureArea y
pictureArea (Translate x y z) = pictureArea z


||| 4. Generic types example

||| Technically generic types are not type. They are actually function that
||| takes type and return a type. See Tree.idr for more details
data Maybe2 a = Nothing2 | Just2 a
