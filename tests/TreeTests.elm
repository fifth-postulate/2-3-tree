module TreeTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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
             , ( [ 1, 2 ], Node3 ( 1, 1 ) ( 2, 1 ) Empty Empty Empty )
             , ( [ 2, 1 ], Node3 ( 1, 1 ) ( 2, 1 ) Empty Empty Empty )
             ]
                |> (List.map <| insertTest (String.fromInt))
            )
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
