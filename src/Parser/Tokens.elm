module Parser.Tokens exposing (and, boolean, ifP, is, leftParen, not, or, proposition, rightParen, thenP, whitespace)

import Parser exposing ((|.), (|=), Parser, Problem(..), keyword, oneOf, succeed, symbol, variable)
import Set


lineComment : Parser ()
lineComment =
    Parser.lineComment "#"


loopIfProgress : Parser a -> Int -> Parser (Parser.Step Int ())
loopIfProgress parser offset =
    succeed identity
        |. parser
        |= Parser.getOffset
        |> Parser.map
            (\newOffset ->
                if offset == newOffset then
                    Parser.Done ()

                else
                    Parser.Loop newOffset
            )


whitespace : Parser ()
whitespace =
    let
        spaces : Parser ()
        spaces =
            Parser.chompWhile (\c -> c == ' ' || c == '\t' || c == '\u{000D}' || c == '\n')
    in
    Parser.loop 0 <|
        loopIfProgress <|
            oneOf
                [ lineComment
                , spaces
                ]


reserved : Set.Set String
reserved =
    Set.fromList [ "True", "true", "False", "false", "not", "and", "or", "is", "if", "then" ]


proposition : Parser String
proposition =
    variable
        { start = Char.isAlphaNum
        , inner = \c -> Char.isAlphaNum c || (c == '_') || (c == '-')
        , reserved = reserved
        }
        |. whitespace


boolean : Parser Bool
boolean =
    oneOf
        [ succeed True |. keyword "True"
        , succeed True |. keyword "true"
        , succeed False |. keyword "False"
        , succeed False |. keyword "false"
        ]
        |. whitespace


keyword_ : String -> Parser ()
keyword_ s =
    keyword s |. whitespace


not : Parser ()
not =
    keyword_ "not"


and : Parser ()
and =
    keyword_ "and"


or : Parser ()
or =
    keyword_ "or"


is : Parser ()
is =
    keyword_ "is"


ifP : Parser ()
ifP =
    keyword_ "if"


thenP : Parser ()
thenP =
    keyword_ "then"


leftParen : Parser ()
leftParen =
    symbol "(" |. whitespace


rightParen : Parser ()
rightParen =
    symbol ")" |. whitespace
