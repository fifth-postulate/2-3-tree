module Tree.Kernel exposing (Tree(..), empty, insert, walk, member)


type Tree a
    = Empty
    | Node2 ( a, Int ) (Tree a) (Tree a)
    | Node3 ( a, Int ) ( a, Int ) (Tree a) (Tree a) (Tree a)


empty : Tree a
empty =
    Empty


insert : comparable -> Tree comparable -> Tree comparable
insert element tree =
    case tree of
        Empty ->
            Node2 ( element, 1 ) Empty Empty

        Node2 ( a, count ) left right ->
            case compare element a of
                LT ->
                    case left of
                        Empty ->
                            Node3 ( element, 1 ) ( a, count ) Empty Empty Empty

                        Node2 _ _ _ ->
                            let
                                subTree =
                                    insert element left
                            in
                            Node2 ( a, count ) subTree right

                        Node3 ( subA, subACount ) ( subB, subBCount ) l m r ->
                            case l of
                                Empty ->
                                    case ( compare element subA, compare element subB ) of
                                        ( LT, _ ) ->
                                            Node3
                                                ( subA, subACount )
                                                ( a, count )
                                                (Node2 ( element, 1 ) Empty Empty)
                                                (Node2 ( subB, subBCount ) Empty Empty)
                                                right

                                        ( EQ, _ ) ->
                                            Node2
                                                ( a, count )
                                                (Node3 ( subA, subACount + 1 ) ( subB, subBCount ) l m r)
                                                right

                                        ( GT, LT ) ->
                                            Node3
                                                ( element, 1 )
                                                ( a, count )
                                                (Node2 ( subA, subACount ) Empty Empty)
                                                (Node2 ( subB, subBCount ) Empty Empty)
                                                right

                                        ( _, EQ ) ->
                                            Node2
                                                ( a, count )
                                                (Node3 ( subA, subACount ) ( subB, subBCount + 1 ) l m r)
                                                right

                                        ( _, GT ) ->
                                            Node3
                                                ( subB, subBCount )
                                                ( a, count )
                                                (Node2 ( subA, subACount ) Empty Empty)
                                                (Node2 ( element, 1 ) Empty Empty)
                                                right

                                _ ->
                                    let
                                        subTree =
                                            insert element left
                                    in
                                    Node2 ( a, count ) subTree right

                EQ ->
                    Node2 ( a, count + 1 ) left right

                GT ->
                    case right of
                        Empty ->
                            Node3 ( a, count ) ( element, 1 ) Empty Empty Empty

                        Node2 _ _ _ ->
                            let
                                subTree =
                                    insert element right
                            in
                            Node2 ( a, count ) left subTree

                        Node3 ( subA, subACount ) ( subB, subBCount ) l m r ->
                            case l of
                                Empty ->
                                    case ( compare element subA, compare element subB ) of
                                        ( LT, _ ) ->
                                            Node3
                                                ( a, count )
                                                ( subA, subACount )
                                                left
                                                (Node2 ( element, 1 ) Empty Empty)
                                                (Node2 ( subB, subBCount ) Empty Empty)

                                        ( EQ, _ ) ->
                                            Node2
                                                ( a, count )
                                                left
                                                (Node3 ( subA, subACount + 1 ) ( subB, subBCount ) l m r)

                                        ( GT, LT ) ->
                                            Node3
                                                ( a, count )
                                                ( element, 1 )
                                                left
                                                (Node2 ( subA, subACount ) Empty Empty)
                                                (Node2 ( subB, subBCount ) Empty Empty)

                                        ( _, EQ ) ->
                                            Node2
                                                ( a, count )
                                                left
                                                (Node3 ( subA, subACount ) ( subB, subBCount + 1 ) l m r)

                                        ( _, GT ) ->
                                            Node3
                                                ( a, count )
                                                ( subB, subBCount )
                                                left
                                                (Node2 ( subA, subACount ) Empty Empty)
                                                (Node2 ( element, 1 ) Empty Empty)

                                _ ->
                                    let
                                        subTree =
                                            insert element left
                                    in
                                    Node2 ( a, count ) subTree right

        Node3 ( a, aCount ) ( b, bCount ) l m r ->
            case ( compare element a, compare element b ) of
                ( LT, _ ) ->
                    case l of
                        Empty ->
                            Node2 ( a, aCount )
                                (Node2 ( element, 1 ) Empty Empty)
                                (Node2 ( b, bCount ) Empty Empty)

                        Node2 _ _ _ ->
                            let
                                subTree =
                                    insert element l
                            in
                            Node3 ( a, aCount ) ( b, bCount ) subTree m r

                        Node3 ( subA, subACount ) ( subB, subBCount ) subL subM subR ->
                            case subL of
                                Empty ->
                                    case ( compare element subA, compare element subB ) of
                                        ( LT, _ ) ->
                                            Node2 ( a, aCount )
                                                (Node2 ( subA, subACount )
                                                    (Node2 ( element, 1 ) Empty Empty)
                                                    (Node2 ( subB, subBCount ) Empty Empty)
                                                )
                                                (Node2 ( b, bCount ) m r)

                                        ( EQ, _ ) ->
                                            Node3 ( a, aCount )
                                                ( b, bCount )
                                                (Node3 ( subA, subACount + 1 ) ( subB, subBCount ) subL subM subR)
                                                m
                                                r

                                        ( GT, LT ) ->
                                            Node2 ( a, aCount )
                                                (Node2 ( element, 1 )
                                                    (Node2 ( subA, subACount ) Empty Empty)
                                                    (Node2 ( subB, subBCount ) Empty Empty)
                                                )
                                                (Node2 ( b, bCount ) m r)

                                        ( _, EQ ) ->
                                            Node3 ( a, aCount )
                                                ( b, bCount )
                                                (Node3 ( subA, subACount ) ( subB, subBCount + 1 ) subL subM subR)
                                                m
                                                r

                                        ( _, GT ) ->
                                            Node2 ( a, aCount )
                                                (Node2 ( subB, subBCount )
                                                    (Node2 ( subA, subACount ) Empty Empty)
                                                    (Node2 ( element, 1 ) Empty Empty)
                                                )
                                                (Node2 ( b, bCount ) m r)

                                _ ->
                                    let
                                        subTree =
                                            insert element l
                                    in
                                    Node3 ( a, aCount ) ( b, bCount ) subTree m r

                ( EQ, _ ) ->
                    Node3 ( a, aCount + 1 ) ( b, bCount ) l m r

                ( GT, LT ) ->
                    case m of
                        Empty ->
                            Node2 ( element, 1 )
                                (Node2 ( a, aCount ) Empty Empty)
                                (Node2 ( b, bCount ) Empty Empty)

                        Node2 _ _ _ ->
                            let
                                subTree =
                                    insert element m
                            in
                            Node3 ( a, aCount ) ( b, bCount ) l subTree r

                        Node3 ( subA, subACount ) ( subB, subBCount ) subL subM subR ->
                            case subM of
                                Empty ->
                                    case ( compare element subA, compare element subB ) of
                                        ( LT, _ ) ->
                                            Node2 ( subA, subACount )
                                                (Node2 ( a, aCount )
                                                    l
                                                    (Node2 ( element, 1 ) Empty Empty)
                                                )
                                                (Node2 ( b, bCount )
                                                    (Node2 ( subB, subBCount ) Empty Empty)
                                                    r
                                                )

                                        ( EQ, _ ) ->
                                            Node3 ( a, aCount )
                                                ( b, bCount )
                                                l
                                                (Node3 ( subA, subACount + 1 ) ( subB, subBCount ) subL subM subR)
                                                r

                                        ( GT, LT ) ->
                                            Node2 ( a, aCount )
                                                (Node2 ( element, 1 )
                                                    l
                                                    (Node2 ( subA, subACount ) Empty Empty)
                                                )
                                                (Node2 ( b, bCount )
                                                    (Node2 ( subB, subBCount ) Empty Empty)
                                                    r
                                                )

                                        ( _, EQ ) ->
                                            Node3 ( a, aCount )
                                                ( b, bCount )
                                                l
                                                (Node3 ( subA, subACount ) ( subB, subBCount + 1 ) subL subM subR)
                                                r

                                        ( _, GT ) ->
                                            Node2 ( subB, subBCount )
                                                (Node2 ( a, aCount )
                                                    l
                                                    (Node2 ( subA, subACount ) Empty Empty)
                                                )
                                                (Node2 ( b, bCount )
                                                    (Node2 ( element, 1 ) Empty Empty)
                                                    r
                                                )

                                _ ->
                                    let
                                        subTree =
                                            insert element m
                                    in
                                    Node3 ( a, aCount ) ( b, bCount ) l subTree r

                ( _, EQ ) ->
                    Node3 ( a, aCount ) ( b, bCount + 1 ) l m r

                ( _, GT ) ->
                    case r of
                        Empty ->
                            Node2 ( b, bCount )
                                (Node2 ( a, aCount ) Empty Empty)
                                (Node2 ( element, 1 ) Empty Empty)

                        Node2 _ _ _ ->
                            let
                                subTree =
                                    insert element r
                            in
                            Node3 ( a, aCount ) ( b, bCount ) l m subTree

                        Node3 ( subA, subACount ) ( subB, subBCount ) subL subM subR ->
                            case subR of
                                Empty ->
                                    case ( compare element subA, compare element subB ) of
                                        ( LT, _ ) ->
                                            Node2 ( b, bCount )
                                                (Node2 ( a, aCount )
                                                    l
                                                    m
                                                )
                                                (Node2 ( subA, subACount )
                                                    (Node2 ( element, 1 ) Empty Empty)
                                                    (Node2 ( subB, subBCount ) Empty Empty)
                                                )

                                        ( EQ, _ ) ->
                                            Node3 ( a, aCount )
                                                ( b, bCount )
                                                l
                                                m
                                                (Node3 ( subA, subACount + 1 ) ( subB, subBCount ) subL subM subR)

                                        ( GT, LT ) ->
                                            Node2 ( b, bCount )
                                                (Node2 ( a, aCount )
                                                    l
                                                    m
                                                )
                                                (Node2 ( element, 1 )
                                                    (Node2 ( subA, subACount ) Empty Empty)
                                                    (Node2 ( subB, subBCount ) Empty Empty)
                                                )

                                        ( _, EQ ) ->
                                            Node3 ( a, aCount )
                                                ( b, bCount )
                                                l
                                                m
                                                (Node3 ( subA, subACount ) ( subB, subBCount + 1 ) subL subM subR)

                                        ( _, GT ) ->
                                            Node2 ( b, bCount )
                                                (Node2 ( a, aCount )
                                                    l
                                                    m
                                                )
                                                (Node2 ( subB, subBCount )
                                                    (Node2 ( subA, subACount ) Empty Empty)
                                                    (Node2 ( element, 1 ) Empty Empty)
                                                )

                                _ ->
                                    let
                                        subTree =
                                            insert element r
                                    in
                                    Node3 ( a, aCount ) ( b, bCount ) l m subTree


member : a -> Tree a -> Bool
member needle haystack =
    case haystack of
        Empty ->
            False

        Node2 ( a, _ ) left right ->
            a == needle || member needle left || member needle right

        Node3 ( a, _ ) ( b, _ ) left middle right ->
            a == needle || b == needle || member needle left || member needle middle || member needle right


walk : (() -> o) -> (( a, Int ) -> o -> o -> o) -> (( a, Int ) -> ( a, Int ) -> o -> o -> o -> o) -> Tree a -> o
walk emptyCase node2Case node3Case tree =
    let
        recurse =
            walk emptyCase node2Case node3Case
    in
    case tree of
        Empty ->
            emptyCase ()

        Node2 data left right ->
            let
                walkedLeft =
                    recurse left

                walkedRight =
                    recurse right
            in
            node2Case data walkedLeft walkedRight

        Node3 aData bData left middle right ->
            let
                walkedLeft =
                    recurse left

                walkedMiddle =
                    recurse middle

                walkedRight =
                    recurse right
            in
            node3Case aData bData walkedLeft walkedMiddle walkedRight
