port module Main exposing (main)

import Browser
import Css exposing (marginLeft, px, rgb)
import Dict
import GraphViz.GraphEvaluation exposing (GraphRenderConfiguration)
import GraphViz.Orientation as Orientation exposing (Orientation(..))
import Html exposing (caption)
import Html.Styled exposing (Html, a, div, input, label, li, p, span, table, td, text, textarea, th, toUnstyled, tr)
import Html.Styled.Attributes exposing (checked, cols, css, rows, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Keyed
import Html.Styled.Lazy
import Interactions.Page exposing (page)
import Interop.URL
import Logic.Assertion exposing (Assertion(..), evaluateAssertions, expandWithModusTollens, propositionNames)
import Logic.AssertionExamples exposing (examples)
import Logic.AssertionGraph
import Logic.Evaluation as Evaluation exposing (Evaluation(..))
import Parser.Assertion
import Parser.Runner
import Set


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


port sendDOT : String -> Cmd msg


port sendHash : String -> Cmd msg


port onHashChange : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    onHashChange (Interop.URL.fromString >> AssertionsFromUrlHash)


type alias Model =
    { assertionsText : String
    , attemptModusTollens : Bool
    , graphConfiguration : GraphRenderConfiguration
    , errors : Maybe (List ( String, List String ))
    , lastValidAssertions : List Logic.Assertion.Assertion
    }


init : a -> ( Model, Cmd msg )
init _ =
    ( { assertionsText = ""
      , attemptModusTollens = False
      , graphConfiguration =
            { orientation = Horizontal
            , showLegend = True
            }
      , lastValidAssertions = []
      , errors = Nothing
      }
    , Cmd.none
    )


type Msg
    = UpdateAssertions String
    | ToggleModusTollens
    | ToggleOrientation
    | ToggleLegend
    | AssertionsFromUrlHash (Maybe String)


graphDOTString : Bool -> GraphRenderConfiguration -> List Logic.Assertion.Assertion -> String
graphDOTString attemptModusTollens config =
    (if attemptModusTollens then
        Logic.Assertion.expandWithModusTollens

     else
        identity
    )
        >> Logic.AssertionGraph.fromAssertions
        >> GraphViz.GraphEvaluation.toDOTString config


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ lastValidAssertions, attemptModusTollens, graphConfiguration } as oldModel) =
    let
        lastValidAssertionsWithNewModel model =
            ( model, lastValidAssertions |> graphDOTString model.attemptModusTollens model.graphConfiguration |> sendDOT )

        updateAssertions assertionsText =
            case Parser.Runner.run Parser.Assertion.assertions assertionsText of
                Ok assertions ->
                    ( { oldModel
                        | assertionsText = assertionsText
                        , errors = Nothing
                        , lastValidAssertions = assertions
                      }
                    , Cmd.batch [ assertionsText |> Interop.URL.toString |> sendHash, assertions |> graphDOTString attemptModusTollens graphConfiguration |> sendDOT ]
                    )

                Err errs ->
                    ( { oldModel
                        | errors = Just errs
                        , assertionsText = assertionsText
                      }
                    , Cmd.none
                    )
    in
    case msg of
        AssertionsFromUrlHash assertionsTextMaybe ->
            case assertionsTextMaybe of
                Nothing ->
                    ( oldModel, Cmd.none )

                Just assertionsText ->
                    if assertionsText == oldModel.assertionsText then
                        ( oldModel, Cmd.none )

                    else
                        updateAssertions assertionsText

        ToggleOrientation ->
            lastValidAssertionsWithNewModel
                { oldModel
                    | graphConfiguration = { graphConfiguration | orientation = Orientation.toggle graphConfiguration.orientation }
                }

        ToggleLegend ->
            lastValidAssertionsWithNewModel
                { oldModel | graphConfiguration = { graphConfiguration | showLegend = not graphConfiguration.showLegend } }

        ToggleModusTollens ->
            lastValidAssertionsWithNewModel { oldModel | attemptModusTollens = not attemptModusTollens }

        UpdateAssertions assertionsText ->
            updateAssertions assertionsText


errorsView : List ( String, List String ) -> Html a
errorsView errors =
    let
        errorView : ( String, List String ) -> ( String, Html a )
        errorView ( first, rest ) =
            ( first
            , li []
                [ text first
                , Html.Styled.Keyed.ul []
                    (rest
                        |> List.map
                            (\orError ->
                                let
                                    str =
                                        "Or " ++ orError
                                in
                                ( str, li [] [ text str ] )
                            )
                    )
                ]
            )
    in
    div [ css [ marginLeft <| px 10 ] ]
        [ p [] [ text "There are some issues with how the assertions are written. If a graph is displayed it represents the most recent correctly formed assertions." ]
        , Html.Styled.Keyed.ul [ css [ Css.color (rgb 255 0 0) ] ] (errors |> List.map errorView)
        ]


rightSpacing : Css.Style
rightSpacing =
    Css.marginRight (px 8)


hyperlink : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
hyperlink =
    Html.Styled.styled a
        [ Css.color (rgb 0 0 255)
        , Css.textDecoration Css.none
        , rightSpacing
        , Css.cursor Css.pointer
        , Css.hover [ Css.textDecoration Css.underline ]
        ]


exampleLink : ( String, String ) -> Html Msg
exampleLink ( caption, example ) =
    hyperlink [ example |> UpdateAssertions |> onClick ] [ text caption ]


checkbox : Bool -> msg -> String -> Html msg
checkbox current click caption =
    span [ css [ rightSpacing ] ]
        [ input
            [ type_ "checkbox"
            , checked current
            , onClick click
            ]
            []
        , label [ onClick click, css [ Css.cursor Css.pointer ] ] [ text caption ]
        ]


symbolTableView : Bool -> List Logic.Assertion.Assertion -> Html a
symbolTableView attemptModusTollens assertions =
    let
        fromEvaluations =
            evaluateAssertions
                (if attemptModusTollens then
                    expandWithModusTollens assertions

                 else
                    assertions
                )

        fromNames =
            propositionNames assertions |> Set.toList |> List.map (\p -> ( p, EUnknown )) |> Dict.fromList

        symbolTable =
            Dict.union fromEvaluations fromNames
    in
    if Dict.isEmpty symbolTable then
        div [] []

    else
        table
            [ css [ Css.display Css.inlineBlock, Css.marginLeft (px 16) ] ]
            (tr []
                ([ "Proposition", "Truth" ] |> List.map (\t -> th [ css [ Css.textAlign Css.left, rightSpacing ] ] [ text t ]))
                :: (symbolTable
                        |> Dict.toList
                        |> List.map
                            (\( proposition, truth ) ->
                                tr [ css [ Css.nthChild "even" [ Css.backgroundColor (rgb 224 224 224) ] ] ]
                                    [ td [] [ text proposition ]
                                    , td [] [ truth |> Evaluation.toString |> text ]
                                    ]
                            )
                   )
            )


view : Model -> Html Msg
view { assertionsText, errors, attemptModusTollens, graphConfiguration, lastValidAssertions } =
    page
        { foreignLinks =
            [ { caption = "My Home Page", url = "https://jsuder-xx.github.io" }
            , { caption = "On GitHub", url = "https://github.com/JSuder-xx/elm.simplication" }
            ]
        , body =
            [ div [ css [ Css.marginBottom (px 6) ] ]
                (span [ css [ rightSpacing ] ] [ text "Examples:" ] :: (examples |> List.map exampleLink))
            , div [ css [ Css.displayFlex ] ]
                [ div []
                    [ textarea
                        [ rows 16
                        , cols 90
                        , css
                            [ Css.backgroundColor (rgb 0 0 0)
                            , Css.fontWeight Css.bold
                            , Css.color (rgb 128 255 160)
                            ]
                        , value assertionsText
                        , onInput UpdateAssertions
                        ]
                        []
                    , div []
                        [ checkbox attemptModusTollens ToggleModusTollens "Try Modus Tollens (Graphs get busy)"
                        , checkbox graphConfiguration.showLegend ToggleLegend "Show Legend"
                        , hyperlink [ onClick ToggleOrientation ]
                            [ graphConfiguration.orientation |> Orientation.toggle |> Orientation.toString |> (\new -> "Change to " ++ new ++ " Layout") |> text
                            ]
                        ]
                    ]
                , errors
                    |> Maybe.map errorsView
                    |> Maybe.withDefault (Html.Styled.Lazy.lazy2 symbolTableView attemptModusTollens lastValidAssertions)
                ]
            ]
        }
