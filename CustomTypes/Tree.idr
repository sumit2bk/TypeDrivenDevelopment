data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left e right) = case compare x e of
                                    LT => Node (insert x left) e right
                                    EQ => orig
                                    GT => Node left e (insert x right)

||| another way to define type. This shows clearly that tree is not a type but a function that takes a type and return another type
||| Note also that we have put a Ord constraint directly on the type itself here.
||| BSTree is a function and "BSTree elem" is a concrete type.
data BSTree : Type -> Type where
  Empty2 : Ord elem => BSTree elem
  Node2: Ord elem => (left: BSTree elem) -> (val: elem) -> (right: BSTree elem) -> BSTree elem
