import Data.Vect

appendImplicit: Vect n elem -> Vect m elem -> Vect (n+m) elem
appendImplicit [] ys = ys
appendImplicit (x :: xs) ys = x :: appendImplicit xs ys

appendExplicit: (elem : Type) -> (n : Nat) -> (m: Nat) -> Vect n elem -> Vect m elem -> Vect (n+m) elem
appendExplicit elem Z m [] ys = ys
appendExplicit elem (S len) m (x :: xs) ys = x :: appendExplicit elem len m xs ys
