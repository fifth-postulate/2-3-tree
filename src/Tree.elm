module Tree exposing (Tree, empty, fromList, insert, toList, member)

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
    let
        emptyCase _ =
            []

        node2Case ( a, aCount ) left right =
            List.concat
                [ left
                , List.repeat aCount a
                , right
                ]

        node3Case (a, aCount) (b, bCount) left middle right =
            List.concat
                [ left
                , List.repeat aCount a
                , middle
                , List.repeat bCount b
                , right
                ]
    in
    Kernel.walk emptyCase node2Case node3Case tree

member : a -> Tree a -> Bool
member needle haystack =
    Kernel.member needle haystack
