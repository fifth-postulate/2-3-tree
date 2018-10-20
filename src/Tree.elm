module Tree exposing (Tree, empty, fromList, insert)

import Tree.Kernel as Kernel


type alias Tree a =
    Kernel.Tree a


empty =
    Kernel.empty


insert : comparable -> Tree comparable -> Tree comparable
insert element tree =
    Kernel.insert element tree


fromList : List comparable -> Tree comparable
fromList =
    List.foldl (\element tree -> insert element tree) empty
