module GraphViz.Orientation exposing (Orientation(..), toString, toggle)


type Orientation
    = Horizontal
    | Vertical


toString : Orientation -> String
toString o =
    case o of
        Horizontal ->
            "Horizontal"

        Vertical ->
            "Vertical"


toggle : Orientation -> Orientation
toggle o =
    case o of
        Horizontal ->
            Vertical

        Vertical ->
            Horizontal
