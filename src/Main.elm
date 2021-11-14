port module Main exposing (main)

import Browser
import Css exposing (marginLeft, px, rgb)
import GraphViz.GraphEvaluation exposing (GraphRenderConfiguration)
import GraphViz.Orientation as Orientation exposing (Orientation(..))
import Html exposing (caption)
import Html.Styled exposing (Html, a, div, input, label, li, p, span, text, textarea, toUnstyled, ul)
import Html.Styled.Attributes exposing (checked, cols, css, rows, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Interactions.Page exposing (page)
import Logic.Assertion
import Logic.AssertionExamples exposing (examples)
import Logic.AssertionGraph
import Parser.Assertion
import Parser.Runner


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port sendDOT : String -> Cmd msg


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


assertionGraph : Bool -> GraphRenderConfiguration -> List Logic.Assertion.Assertion -> String
assertionGraph attemptModusTollens config =
    (if attemptModusTollens then
        Logic.Assertion.expandWithModusTollens

     else
        identity
    )
        >> Logic.AssertionGraph.fromAssertions
        >> GraphViz.GraphEvaluation.toDOTString config


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ lastValidAssertions, attemptModusTollens, graphConfiguration } as oldModel) =
    case msg of
        ToggleOrientation ->
            let
                newConfiguration =
                    { graphConfiguration | orientation = Orientation.toggle graphConfiguration.orientation }
            in
            ( { oldModel | graphConfiguration = newConfiguration }
            , lastValidAssertions |> assertionGraph attemptModusTollens newConfiguration |> sendDOT
            )

        ToggleLegend ->
            let
                newConfiguration =
                    { graphConfiguration | showLegend = not graphConfiguration.showLegend }
            in
            ( { oldModel | graphConfiguration = newConfiguration }
            , lastValidAssertions |> assertionGraph attemptModusTollens newConfiguration |> sendDOT
            )

        ToggleModusTollens ->
            ( { oldModel | attemptModusTollens = not attemptModusTollens }
            , lastValidAssertions |> assertionGraph (not attemptModusTollens) graphConfiguration |> sendDOT
            )

        UpdateAssertions assertionsText ->
            case Parser.Runner.run Parser.Assertion.assertions assertionsText of
                Ok assertions ->
                    ( { oldModel
                        | assertionsText = assertionsText
                        , errors = Nothing
                        , lastValidAssertions = assertions
                      }
                    , assertions |> assertionGraph attemptModusTollens graphConfiguration |> sendDOT
                    )

                Err errs ->
                    ( { oldModel
                        | errors = Just errs
                        , assertionsText = assertionsText
                      }
                    , lastValidAssertions |> assertionGraph attemptModusTollens graphConfiguration |> sendDOT
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
    div [ css [ marginLeft <| px 10 ] ]
        [ p [] [ text "There are some issues with how the assertions are written. If a graph is displayed it represents the most recent correctly formed assertions." ]
        , ul [ css [ Css.color (rgb 255 0 0) ] ] (errors |> List.map errorView)
        ]


emptyDiv : Html msg
emptyDiv =
    div [] []


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


view : Model -> Html Msg
view { assertionsText, errors, attemptModusTollens, graphConfiguration } =
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
                    |> Maybe.withDefault emptyDiv
                ]
            ]
        }
