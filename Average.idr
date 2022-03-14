module Average

||| Calculate the average length of words in a string.
||| @str a string containing words separated by whitespace.
export
average : (str: String) -> Double
average str = let numWords = wordCount str
                  totalLength = sum (allLengths (words str)) in
                    cast totalLength / cast numWords
              where 
                wordCount : String -> Nat
                wordCount s = length (words s)

                allLengths : List String -> List Nat
                allLengths l = map length l


                  