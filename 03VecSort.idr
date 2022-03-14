import Data.Vect

insert : Ord elem => (x : elem) -> (sorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs else y :: insert x xs

insSort: Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let sorted = insSort xs in
                      insert x sorted
