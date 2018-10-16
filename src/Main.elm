module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as Event
import Tree exposing (Tree, empty, insert)


main =
    Browser.sandbox
        { init = emptyModel
        , update = update
        , view = view Debug.toString
        }


emptyModel : Model Int
emptyModel =
    { tree = empty, n = Nothing, message = Nothing }


type alias Model a =
    { tree : Tree a, n : Maybe Int, message : Maybe String }


type Message
    = Update String
    | Insert


update : Message -> Model Int -> Model Int
update msg model =
    case msg of
        Update value ->
            let
                n =
                    value
                        |> String.toInt

                message =
                    n
                        |> Maybe.map (\m -> "entered: " ++ String.fromInt m)
                        |> Maybe.withDefault "not a number"
            in
            { model | n = n, message = Just message }

        Insert ->
            let
                tree =
                    model.n
                        |> Maybe.map (\n -> insert n model.tree)
                        |> Maybe.withDefault model.tree
            in
            { model | tree = tree, n = Nothing, message = Nothing }


view : (Tree a -> String) -> Model a -> Html Message
view toString model =
    let
        text =
            model.message
                |> Maybe.withDefault ""
    in
    Html.div []
        [ Html.div []
            [ Html.span [] [ Html.text text ]
            ]
        , Html.div []
            [ Html.input [ Event.onInput Update ] []
            , Html.button [ Event.onClick Insert ] [ Html.text "insert" ]
            ]
        , Html.pre [] [ Html.text (toString model.tree) ]
        ]
