import Data.Vect

-- Type synonym functions
Position: Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

-- Type level Fucntion.
StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

-- An exmaple of type level fucntions getting used in function signature
valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False x = x
valToString True x = cast x
