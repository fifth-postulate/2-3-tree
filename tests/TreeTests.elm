module TreeTests exposing (suite)

import Combinatorics
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, map3, string)
import Test exposing (..)
import Tree exposing (empty, insert)
import Tree.Kernel exposing (Tree(..))


suite : Test
suite =
    describe "Tree module"
        [ describe "empty"
            [ test "empty produces an Empty tree." <|
                \_ ->
                    let
                        actual =
                            empty

                        expected =
                            Empty
                    in
                    Expect.equal actual expected
            ]
        , describe "insert"
            ([ ( [ 1 ], Node2 ( 1, 1 ) Empty Empty )
             , ( [ 1, 1 ], Node2 ( 1, 2 ) Empty Empty )
             , ( [ 1, 2 ], Node3 ( 1, 1 ) ( 2, 1 ) Empty Empty Empty )
             , ( [ 2, 1 ], Node3 ( 1, 1 ) ( 2, 1 ) Empty Empty Empty )
             , ( [ 1, 3, 0 ], Node2 ( 1, 1 ) (Node2 ( 0, 1 ) Empty Empty) (Node2 ( 3, 1 ) Empty Empty) )
             , ( [ 1, 3, 2 ], Node2 ( 2, 1 ) (Node2 ( 1, 1 ) Empty Empty) (Node2 ( 3, 1 ) Empty Empty) )
             , ( [ 1, 3, 4 ], Node2 ( 3, 1 ) (Node2 ( 1, 1 ) Empty Empty) (Node2 ( 4, 1 ) Empty Empty) )
             , ( [ 1, 3, 4, 0 ], Node2 ( 3, 1 ) (Node3 ( 0, 1 ) ( 1, 1 ) Empty Empty Empty) (Node2 ( 4, 1 ) Empty Empty) )
             , ( [ 1, 3, 4, 0, 5 ], Node2 ( 3, 1 ) (Node3 ( 0, 1 ) ( 1, 1 ) Empty Empty Empty) (Node3 ( 4, 1 ) ( 5, 1 ) Empty Empty Empty) )
             , ( [ 1, 3, 4, 0, 5, 2 ], Node3 ( 1, 1 ) ( 3, 1 ) (Node2 ( 0, 1 ) Empty Empty) (Node2 ( 2, 1 ) Empty Empty) (Node3 ( 4, 1 ) ( 5, 1 ) Empty Empty Empty) )
             , ( [ 1, 3, 4, 0, 5, 2, 6 ], Node2 ( 3, 1 ) (Node2 ( 1, 1 ) (Node2 ( 0, 1 ) Empty Empty) (Node2 ( 2, 1 ) Empty Empty)) (Node2 ( 5, 1 ) (Node2 ( 4, 1 ) Empty Empty) (Node2 ( 6, 1 ) Empty Empty)) )
             ]
                |> (List.map <| insertTest String.fromInt)
            )
        , describe "from fuzz to test"
            [ test "repeated elements should be recorder correctly" <|
                \_ ->
                    let
                        actual =
                            Tree.fromList [ 3, 4, 0, 1, 2, 4 ]

                        expected =
                            Node3 ( 1, 1 ) ( 3, 1 ) (Node2 ( 0, 1 ) Empty Empty) (Node2 ( 2, 1 ) Empty Empty) (Node2 ( 4, 2 ) Empty Empty)
                    in
                    Expect.equal expected actual
            ]
        , fuzz (map3 toList3 int int int) "order of insertion with 3 elements does not matter" <|
            \aList ->
                let
                    aTree =
                        Tree.fromList aList

                    same =
                        aList
                            |> Combinatorics.permutationsOf
                            |> List.map Tree.fromList
                            |> List.all (\tree -> tree == aTree)
                in
                Expect.true "all trees should be the same" same

        -- , fuzz (list int) "fromList toList sorts the original list" <|
        --     \aList ->
        --         let
        --             aTree =
        --                 Tree.fromList aList
        --             actual =
        --                 Tree.toList aTree
        --             expected =
        --                 List.sort aList
        --         in
        --             Expect.equal actual expected
        ]


insertTest : (comparable -> String) -> ( List comparable, Tree comparable ) -> Test
insertTest toString ( toInsert, expected ) =
    let
        description =
            toInsert
                |> List.map toString
                |> String.join ", "

        testName =
            "insert "
                ++ description
                ++ " into an empty tree should create an expected tree"
    in
    test testName <|
        \_ ->
            let
                actual =
                    List.foldl (\element tree -> insert element tree) empty toInsert
            in
            Expect.equal actual expected


toList3 : a -> a -> a -> List a
toList3 a b c =
    [ a, b, c ]
