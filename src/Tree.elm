module Tree exposing (Tree, empty, fromList, insert, toList)

import Tree.Kernel as Kernel


type alias Tree a =
    Kernel.Tree a


empty =
    Kernel.empty


insert : comparable -> Tree comparable -> Tree comparable
insert element tree =
    Kernel.insert element tree


fromList : List comparable -> Tree comparable
fromList aList =
    List.foldl (\element tree -> insert element tree) empty aList


toList : Tree a -> List a
toList tree =
    Kernel.toList tree
