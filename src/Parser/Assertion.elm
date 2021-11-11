module Parser.Assertion exposing (assertion, assertions)

import Logic.Assertion exposing (Assertion(..))
import Logic.BooleanExpression exposing (BooleanExpression(..))
import Parser exposing ((|.), (|=), Parser, Step(..), andThen, end, oneOf, succeed)
import Parser.BooleanExpression
import Parser.Tokens as Tokens exposing (whitespace)


truthOfProposition : Parser Assertion
truthOfProposition =
    consequent |> Parser.map (\( p, t ) -> TruthOfProposition p t)


consequent : Parser ( String, Bool )
consequent =
    oneOf
        [ succeed (\p -> ( p, False )) |. Tokens.not |= Tokens.proposition
        , succeed (\p -> ( p, True )) |= Tokens.proposition
        ]


consequentList : Parser (List ( String, Bool ))
consequentList =
    consequent |> andThen (consequentHelp [])


consequentHelp : List ( String, Bool ) -> ( String, Bool ) -> Parser (List ( String, Bool ))
consequentHelp consequents currentConsequent =
    oneOf
        [ succeed identity
            |. Tokens.comma
            |= consequent
            |> andThen (\newConsequent -> consequentHelp (currentConsequent :: consequents) newConsequent)
        , Parser.lazy <| \_ -> succeed <| List.reverse <| currentConsequent :: consequents
        ]


implication : Parser Assertion
implication =
    succeed Implication
        |. Tokens.ifP
        |= Parser.BooleanExpression.parser
        |. Tokens.thenP
        |= consequentList


assertion : Parser Assertion
assertion =
    succeed identity
        |. whitespace
        |= oneOf
            [ implication
            , truthOfProposition
            ]


assertions : Parser (List Assertion)
assertions =
    Parser.loop [] assertionsHelp


assertionsHelp : List Assertion -> Parser (Step (List Assertion) (List Assertion))
assertionsHelp assertionsAccum =
    oneOf
        [ assertion |> Parser.map (\a -> Loop (a :: assertionsAccum))
        , end |> Parser.map (\_ -> Done <| List.reverse assertionsAccum)
        ]
