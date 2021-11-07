module Parser.BooleanExpression exposing (parser)

import Logic.BooleanExpression exposing (BooleanExpression(..))
import Parser exposing ((|.), (|=), Parser, andThen, lazy, oneOf, succeed)
import Parser.Tokens as Tokens


term : Parser BooleanExpression
term =
    oneOf
        [ succeed Not |. Tokens.not |= Parser.lazy (\_ -> term)
        , succeed Proposition |= Tokens.proposition
        , succeed identity
            |. Tokens.leftParen
            |= lazy (\_ -> parser)
            |. Tokens.rightParen
        ]


parser : Parser BooleanExpression
parser =
    term |> andThen (expressionHelp [])


type Op
    = AndOp
    | OrOp


op : Parser Op
op =
    oneOf
        [ succeed AndOp |. Tokens.and
        , succeed OrOp |. Tokens.or
        ]


expressionHelp : List ( BooleanExpression, Op ) -> BooleanExpression -> Parser BooleanExpression
expressionHelp reversedAccum expr =
    oneOf
        [ succeed Tuple.pair
            |= op
            |= term
            |> andThen (\( operator, newExpr ) -> expressionHelp (( expr, operator ) :: reversedAccum) newExpr)
        , lazy (\_ -> succeed (finalize reversedAccum expr))
        ]


finalize : List ( BooleanExpression, Op ) -> BooleanExpression -> BooleanExpression
finalize reversedAccum finalExpr =
    case reversedAccum of
        [] ->
            finalExpr

        ( expr, AndOp ) :: remaining ->
            finalize remaining (And [ expr, finalExpr ])

        ( expr, OrOp ) :: remaining ->
            finalize remaining (Or [ expr, finalExpr ])
