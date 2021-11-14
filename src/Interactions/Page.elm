module Interactions.Page exposing (page)

import Css exposing (float, left, px, rgb)
import Html exposing (caption)
import Html.Styled exposing (Html, a, div, nav, text)
import Html.Styled.Attributes exposing (css, href, target)
import Html.Styled.Lazy


heightOfTopBar : number
heightOfTopBar =
    36


topMenuBarView : List { caption : String, url : String } -> Html a
topMenuBarView links =
    let
        menuLink { caption, url } =
            a
                [ target "_blank"
                , href url
                , css
                    [ float left
                    , Css.color <| Css.rgb 255 255 255
                    , Css.textAlign Css.center
                    , Css.padding2 (px 10) (px 16)
                    , Css.textDecoration Css.none
                    , Css.fontSize (px 14)
                    , Css.hover [ Css.textDecoration Css.underline ]
                    ]
                ]
                [ text caption ]
    in
    nav
        [ css
            [ Css.position Css.fixed
            , Css.top (px 0)
            , Css.height (px heightOfTopBar)
            , Css.width (Css.pct 100)
            , Css.backgroundColor (rgb 40 40 40)
            , Css.overflow Css.hidden
            , Css.fontFamily Css.sansSerif
            ]
        ]
        (List.map menuLink links)


page : { foreignLinks : List { caption : String, url : String }, body : List (Html msg) } -> Html msg
page { foreignLinks, body } =
    div []
        [ Html.Styled.Lazy.lazy topMenuBarView foreignLinks
        , div
            [ css [ Css.marginTop (px (heightOfTopBar + 1)), Css.padding (px 6) ] ]
            body
        ]
