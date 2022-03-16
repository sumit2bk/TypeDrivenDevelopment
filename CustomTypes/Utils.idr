module Utils
import Data.Vect

export
tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} x xs = case integerToFin x n of
                     Nothing => Nothing
                     (Just idx) => Just (index idx xs)
