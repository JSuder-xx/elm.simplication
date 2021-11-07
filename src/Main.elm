port module Main exposing (main)

import Browser
import GraphViz.GraphEvaluation exposing (Freshness(..))
import Html exposing (Html, div, li, p, text, textarea, ul)
import Html.Attributes exposing (cols, rows, style, value)
import Html.Events exposing (onClick, onInput)
import Logic.AssertionExamples exposing (examples)
import Logic.AssertionGraph
import Parser.Assertion
import Parser.Runner


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { assertions = "", lastValidDotProducer = Nothing, errors = Nothing }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port sendDOT : String -> Cmd msg


type alias Model =
    { assertions : String
    , errors : Maybe (List ( String, List String ))
    , lastValidDotProducer : Maybe (Freshness -> String)
    }


type Msg
    = UpdateAssertions String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { lastValidDotProducer } =
    case msg of
        UpdateAssertions assertions ->
            let
                dotProducerResult =
                    Parser.Runner.run Parser.Assertion.assertions assertions
                        |> Result.map (Logic.AssertionGraph.fromAssertions >> GraphViz.GraphEvaluation.toDOTString)
            in
            case dotProducerResult of
                Ok dotProducer ->
                    ( { assertions = assertions
                      , errors = Nothing
                      , lastValidDotProducer = Just dotProducer
                      }
                    , sendDOT (dotProducer Fresh)
                    )

                Err errs ->
                    ( { assertions = assertions
                      , errors = Just errs
                      , lastValidDotProducer = lastValidDotProducer
                      }
                    , sendDOT
                        (case lastValidDotProducer of
                            Just producer ->
                                producer Stale

                            Nothing ->
                                ""
                        )
                    )


errorsView : List ( String, List String ) -> Html a
errorsView errors =
    let
        errorView : ( String, List String ) -> Html a
        errorView ( first, rest ) =
            li []
                [ text first
                , ul []
                    (rest |> List.map (\orError -> li [] [ text <| "Or " ++ orError ]))
                ]
    in
    div [ style "margin-left" "10px" ]
        [ p [] [ text "There are some issues with how the assertions are written. If a graph is displayed it represents the most recent correctly formed assertions." ]
        , ul [ style "color" "#a00000" ] (errors |> List.map errorView)
        ]


emptyDiv : Html msg
emptyDiv =
    div [] []


exampleLink : ( String, String ) -> Html Msg
exampleLink ( caption, example ) =
    Html.a
        [ example |> UpdateAssertions |> onClick
        , style "color" "#00f"
        , style "text-decoration" "underline"
        , style "cursor" "pointer"
        , style "margin-right" "16px"
        ]
        [ Html.text caption ]


view : Model -> Html Msg
view { assertions, errors } =
    div []
        [ div [ style "margin-bottom" "8px" ]
            (Html.span [ style "margin-right" "8px" ] [ text "Examples:" ] :: (examples |> List.map exampleLink))
        , div [ style "display" "flex" ]
            [ div []
                [ textarea
                    [ rows 16
                    , cols 90
                    , style "background-color" "black"
                    , style "font-weight" "bold"
                    , style "color" "#80ffd0"
                    , value assertions
                    , onInput UpdateAssertions
                    ]
                    []
                ]
            , errors
                |> Maybe.map errorsView
                |> Maybe.withDefault emptyDiv
            ]
        ]