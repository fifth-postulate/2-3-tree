module Tree exposing
    ( Tree
    , empty, fromList, insert
    , member
    , toList
    )

{-| The `Tree` module provides the [2-3 tree](https://en.wikipedia.org/wiki/2%E2%80%933_tree) data-structure.

A 2-3 tree is a

> a tree data structure, where every node with children (internal node) has either two children (2-node) and one data element or three children (3-nodes) and two data elements. According to Knuth, "a B-tree of order 3 is a 2-3 tree." Nodes on the outside of the tree (leaf nodes) have no children and one or two data elements.

A 2-3 tree is self-balancing.


# Types

@docs Tree


# Construction

@docs empty, fromList, insert

The most convenient way to construct a `Tree` is with the `fromList` method.

    tree =
        Tree.fromList [ 1, 2, 3, 4, 5, 6 ]

If the elements in the tree are not known at the time of construction on could use the `empty` constructor, and use `insert` to add elements.

    newTree =
        Tree.insert 7 tree


# Utilities

@docs member


# Conversion

@docs toList

-}

import Tree.Kernel as Kernel


{-| The type defining a 2-3 tree.
-}
type alias Tree a =
    Kernel.Tree a


{-| Creates an empty 2-3 tree.
-}
empty =
    Kernel.empty


{-| Inserts an element into a 2-3 tree.
-}
insert : comparable -> Tree comparable -> Tree comparable
insert element tree =
    Kernel.insert element tree


{-| Creates an 2-3 tree from a list of elements.
-}
fromList : List comparable -> Tree comparable
fromList aList =
    List.foldl (\element tree -> insert element tree) empty aList


{-| Returns all the members of this tree.

Note that

    Tree.toList <| Tree.fromList list

will sort the `list`.

-}
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

        node3Case ( a, aCount ) ( b, bCount ) left middle right =
            List.concat
                [ left
                , List.repeat aCount a
                , middle
                , List.repeat bCount b
                , right
                ]
    in
    Kernel.walk emptyCase node2Case node3Case tree


{-| Determines if an element is found in the 2-3 tree.
-}
member : a -> Tree a -> Bool
member needle haystack =
    Kernel.member needle haystack
