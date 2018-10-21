module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
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
    { tree = empty, input = Nothing }


type alias Model a =
    { tree : Tree a, input : Maybe String }


type Message
    = Update String
    | Insert


update : Message -> Model Int -> Model Int
update msg model =
    case msg of
        Update value ->
            let
                input =
                    case value of
                        "" ->
                            Nothing

                        _ ->
                            Just value
            in
            { model | input = input }

        Insert ->
            let
                tree =
                    model.input
                        |> Maybe.andThen String.toInt
                        |> Maybe.map (\n -> insert n model.tree)
                        |> Maybe.withDefault model.tree
            in
            { model | tree = tree, input = Nothing }


view : (Tree a -> String) -> Model a -> Html Message
view toString model =
    let
        n =
            model.input
                |> Maybe.andThen String.toInt

        text =
            n
                |> Maybe.map (\_ -> "")
                |> Maybe.withDefault "Enter a number"

        value =
            model.input
                |> Maybe.withDefault ""

        disabled =
            n
                |> Maybe.map (\_ -> False)
                |> Maybe.withDefault True
    in
    Html.div []
        [ Html.div []
            [ Html.span [] [ Html.text text ]
            ]
        , Html.div []
            [ Html.input [ Event.onInput Update, Attribute.value value ] []
            , Html.button [ Event.onClick Insert, Attribute.disabled disabled ] [ Html.text "insert" ]
            ]
        , Html.pre [] [ Html.text (toString model.tree) ]
        ]
