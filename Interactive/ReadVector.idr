import Data.Vect

readVectorLen : (len : Nat) -> IO (Vect len String)
readVectorLen Z = pure []
readVectorLen (S k) = do
   x <- getLine
   xs <- readVectorLen k
   pure (x :: xs)

data VectUnknown : Type -> Type where
  MkVect : (len: Nat) -> Vect len a -> VectUnknown a


readVect : IO (VectUnknown String)
readVect = do
  x <- getLine
  if x == ""
    then pure (MkVect _ [])
    else do
      MkVect _ xs <- readVect
      pure (MkVect _ (x :: xs))


-- Idris sports dependent pairs for above scenarios
-- So you don't have defines custom types like VectUnknown
-- you can define above function using dependent pairs

readVect2 : IO (n ** Vect n String)
readVect2 = do
  x <- getLine
  if (x == "")
    then pure (_ ** [])
    else do
      (_ ** xs) <- readVect2
      pure (_ ** (x :: xs))

zipInputs : IO ()
zipInputs = do
  putStrLn "Enter first vector (blank line to end):"
  (len1 ** vec1) <- readVect2
  putStrLn "Enter second vector (blank line to end):"
  (len2 ** vec2) <- readVect2
  case exactLength len1 vec2 of
   Nothing => putStrLn "Vectors are of different Lenght"
   Just vec2' => printLn (zip vec1 vec2')
