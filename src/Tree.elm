module Tree exposing (Tree, empty, insert)

import Tree.Kernel as Kernel


type alias Tree a =
    Kernel.Tree a


empty =
    Kernel.empty


insert : comparable -> Tree comparable -> Tree comparable
insert element tree =
    Kernel.insert element tree
