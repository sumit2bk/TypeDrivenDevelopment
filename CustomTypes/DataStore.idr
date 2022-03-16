module main

import Data.Vect
import CustomTypes.Utils

data DataStore : Type where
  MkStore : (size: Nat) -> (items: Vect size String) -> DataStore

size: DataStore -> Nat
size (MkStore size' items) = size'

items: (store: DataStore) -> Vect (size store) String
items (MkStore size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkStore size items) newItem = MkStore _ (addToData newItem items)
  where addToData : String -> Vect old String -> Vect (S old) String
        addToData item [] = [item]
        addToData item (x::xs) = x :: addToData item xs


data Command = Add String | Get Integer | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand cmd args = case cmd of
                             "add" => Just (Add args)
                             "get" => case all isDigit (unpack args) of
                                        False => Nothing
                                        True => Just (Get (cast args))
                             "quit" => Just Quit
                             _ => Nothing

parse: String -> Maybe Command
parse x = case span (/= ' ') x of
               (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (idx : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry idx store = case tryIndex idx (items store) of
                          Nothing => Just ("Invalid Index\n", store)
                          (Just x) => Just ("Value" ++ show x ++ "\n", store)

addDataToStore : DataStore -> String -> Maybe (String, DataStore)
addDataToStore store args = case parse args of
                                    Nothing => Just ("Invalid command\n", store)
                                    (Just (Add item)) => Just ("Id " ++ show (size store + 1) ++ "\n" , addToStore store item)
                                    (Just (Get idx)) => getEntry idx store
                                    (Just Quit) => Nothing


main : IO ()
main = replWith (MkStore _ []) "Command " addDataToStore
