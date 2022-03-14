module WordLengthVec
import Data.Vect

allLengths: Vect n String -> Vect n Nat
allLengths [] = []
allLengths (x :: xs) = x :: allLengths xs
