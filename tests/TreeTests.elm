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
            [ test "insert a value into an empty tree" <|
                  \_ ->
                  let
                      actual =
                          empty
                              |> insert 1

                      expected =
                          Node2 (1, 1) Empty Empty
                  in
                      Expect.equal actual expected
            ]
        ]
