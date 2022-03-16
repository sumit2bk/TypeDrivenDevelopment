data Vect: Nat -> Type -> Type where
  Nil: Vect Z a
  (::): (x:a) -> (xs: Vect k a) -> Vect (S k) a

append: Vect m a -> Vect n a -> Vect (n+m) a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys
