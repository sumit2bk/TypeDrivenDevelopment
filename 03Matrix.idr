import Data.Vect

addMatrix : Num numTypes => Vect rows (Vect cols numTypes) -> Vect rows (Vect cols numTypes) -> Vect rows (Vect cols numTypes)

multiplyMatrix : Num numTypes => Vect m (Vect n numTypes) -> Vect n (Vect p numTypes) -> Vect m (Vect p numTypes)

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transponseMatrix' : (x : Vect n elem) -> (transpose : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transponseMatrix' [] [] = []
transponseMatrix' (x :: xs) (y :: ys) = (x :: y) :: transponseMatrix' xs ys

transposeMatrix : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrix [] = createEmpties
transposeMatrix (x :: xs) = let transpose = transposeMatrix xs in
                              transponseMatrix' x transpose
