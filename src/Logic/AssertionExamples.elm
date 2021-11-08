module Logic.AssertionExamples exposing (..)


lines : List String -> String
lines =
    String.join "\n"


examples : List ( String, String )
examples =
    [ ( "Just the Facts"
      , [ "# Lines starting with the # character are comments"
        , ""
        , "# Below are examples of asserting the truth of various 'propositions'."
        , "# A proposition can be any text starting with a letter, followed by letters, numbers, "
        , "# dashes (-) and underscores (_). Propositions may not include spaces (sorry!). "
        , "grass-is-green"
        , "roses-are-red"
        , "WaterCanBeASolidLiquidOrGas"
        , ""
        , "# Indicate that a proposition is false by placing a 'not' in front"
        , "not unicorns_exists"
        ]
            |> lines
      )
    , ( "Implications (People Are Strange)"
      , [ "# Lines starting with the # character are comments"
        , ""
        , "# The three lines below assert 'if this then that' relationships or implications."
        , "# Implications do NOT assert the truth of 'this' or 'that' individually. They"
        , "# communicate if 'this' is true then 'that' must be true. Both could be false/unknown. "
        , "# In addition, even if 'this' is false 'that' could be true for other reasons. "
        , ""
        , "if you-are-a-stranger then people-are-strange"
        , "if you-are-a-stranger then you-are-alone"
        , "if you-are-alone then people-seem-wicked"
        , ""
        , "# The commented line below makes a statement regarding the truth of you-are-a-stranger."
        , "# Remove the leading # character to see what happens in the diagram."
        , ""
        , "# you-are-a-stranger"
        ]
            |> lines
      )
    , ( "Implications 2"
      , [ "# A single if/then can have multiple consequences which are separated by a comma."
        , "if A then Q, R, S"
        , ""
        , "# You can place a 'not' in front of a proposition that should become false."
        , "if Q then not X, not Y, Z"
        ]
            |> lines
      )
    , ( "Contradiction"
      , [ "# The two lines below state a syllogism"
        , "if a then b"
        , "if b then c"
        , ""
        , "# In asserting that 'a' is true we also know 'b' and 'c' are also true."
        , "a"
        , ""
        , "# Remove the leading # from the line below to trigger a contradiction."
        , "# if c then not a"
        ]
            |> lines
      )
    , ( "Irrelevant (I Like Turtles)"
      , [ "# In this example we have a syllogism regarding spiders and black widows."
        , "if spider then has-eight-legs"
        , "if black-widow then spider"
        , ""
        , "# ...and we have a proposition that does not affect the outcome or truth."
        , "# We can clearly see in the diagram that 'I-like-turtles' is not connected to anything."
        , "I-like-turtles"
        ]
            |> lines
      )
    , ( "Complex"
      , [ "# Here we see the use of 'not', 'or', 'and', and '()' in the 'antecedent' part of the if."
        , "# In 'if this then that', 'this' is the antecedent and 'that' is the consequent."
        , "# Don't worry, you won't be tested on this! :) "
        , "# In the consequent (that-part) you can list multiple propositions that should be true/false"
        , "# joined by a comma. You indicate the proposition should be false by placing a"
        , "# 'not' before it."
        , ""
        , "if (a or not b) and (c or d) then q, not r, s"
        , ""
        , "if c and s then x"
        , ""
        , "# Try playing around with what is true and what is false."
        , "not b"
        , "c"
        ]
            |> lines
      )
    ]
