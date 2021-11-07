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
        , "grass-is-green is true"
        , "roses-are-red is true"
        , "unicorns_exists is false"
        , "WaterCanBeASolidLiquidOrGas is true"
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
        , "# you-are-a-stranger is true"
        ]
            |> lines
      )
    , ( "Contradiction"
      , [ "# The two lines below state a syllogism"
        , "if a then b"
        , "if b then c"
        , ""
        , "# In asserting that 'a' is true we also know 'b' and 'c' are also true."
        , "a is true"
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
        , "I-like-turtles is true"
        ]
            |> lines
      )
    , ( "Complex"
      , [ "# Here we see the use of 'not', 'or', 'and', and '()' in the 'antecedent' part of the if."
        , "# In 'if this then that', this is the 'antecedent' and 'that' is the consequent."
        , "# Don't worry, you won't be tested on this! :) "
        , "# In the consequent (that-part) you can list multiple propositions that should be true/false"
        , "# joined by the word 'and'. You indicate the proposition should be false by placing a"
        , "# 'not' before it. UNFORTUNATELY, you cannot use 'not', 'or', and '()' in the consequent;"
        , "# only in the antecedent."
        , ""
        , "if (a or not b) and (c or d) then q and r and not s"
        , ""
        , "if c and s then x"
        , ""
        , "# Try playing around with what is true and what is false."
        , "b is false"
        , "c is true"
        ]
            |> lines
      )
    ]
