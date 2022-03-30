-- creating a type that can take variable number of arguments
AdderType : Nat -> Type
AdderType Z = Int
AdderType (S k) = Int -> AdderType k

adder : (n: Nat) -> (acc: Int) -> AdderType n
adder Z acc = acc
adder (S k) acc = (\x => adder k (acc+x))


-- Generic adder Type that works for any type not just intToBool
AdderType2 : Nat -> Type -> Type
AdderType2 Z type = type
AdderType2 (S k) type = type -> AdderType2 k type

adder2: Num numType => (n: Nat) -> (acc: numType) -> AdderType2 n numType
adder2 Z acc = acc
adder2 (S k) acc = (\x => adder2 k (acc+x))


data Format = Number Format | Str Format | Lit String Format | End

PrintfType: Format -> Type
PrintfType (Number x) = Int -> PrintfType x
PrintfType (Str x) = String -> PrintfType x
PrintfType (Lit x y) = PrintfType y
PrintfType End = String

printfFmt: (fmt: Format) -> (acc: String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt  (acc ++ show i)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Lit str fmt) acc = printfFmt fmt (acc ++ str)
printfFmt End acc = acc

toFormat: (xs : List Char) -> Format
toFormat [] = End
toFormat ('%':: 'd' :: chars) = Number (toFormat chars)
toFormat ('%':: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                          Lit lit chars' => Lit (strCons c lit) chars'
                          fmt => Lit (strCons c "") fmt


printf: (fmt: String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
