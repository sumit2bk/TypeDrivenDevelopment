
import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do
  Just num1 <- readNumber | Nothing => pure Nothing
  Just num2 <- readNumber | Nothing => pure Nothing
  pure (Just (num1,num2))


conundown: Nat -> IO ()
conundown Z = putStrLn "Lift off!"
conundown (S secs) = do
  putStrLn (show (S secs))
  usleep 10000000
  conundown secs
