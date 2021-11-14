module GraphViz.GraphEvaluation exposing (GraphRenderConfiguration, toDOTString)

import Data.Graph exposing (Edge(..), Graph, Node(..), NodeId, edges, nodeId, nodes)
import GraphViz.Orientation exposing (Orientation(..))
import Logic.AssertionGraph exposing (NodeType(..))
import Logic.Evaluation exposing (Evaluation(..))


quoted : String -> String
quoted s =
    "\"" ++ s ++ "\""


quotedProperty : ( String, String ) -> String
quotedProperty ( propertyName, propertyValue ) =
    propertyName ++ " = " ++ quoted propertyValue


quotedProperties : List ( String, String ) -> String
quotedProperties =
    List.map quotedProperty >> String.join ";\n"


evaluationToFillFontColors : Evaluation -> ( String, String )
evaluationToFillFontColors evaluation =
    case evaluation of
        EUnknown ->
            ( "#909090", "#000000" )

        EFalse ->
            ( "#000000", "#c0c0c0" )

        ETrue ->
            ( "#ffffff", "#000000" )

        EContradiction ->
            ( "#ff0000", "#ffffff" )


genericNode : { id : String, label : String, evaluation : Evaluation, shape : String, fontSize : String } -> String
genericNode { id, label, evaluation, shape, fontSize } =
    let
        ( fillcolor, fontcolor ) =
            evaluationToFillFontColors evaluation
    in
    [ id
    , " ["
    , quotedProperties
        [ ( "label", label )
        , ( "style", "filled" )
        , ( "fillcolor", fillcolor )
        , ( "fontcolor", fontcolor )
        , ( "fontsize", fontSize )
        , ( "fontname", "Arial" )
        , ( "shape", shape )
        ]
    , "]"
    ]
        |> String.join ""


nodeIdToString : NodeId -> String
nodeIdToString =
    nodeId >> String.fromInt


edgeToString : Edge -> String
edgeToString (Edge fromNodeId toNodeId) =
    ([ fromNodeId, toNodeId ]
        |> List.map nodeIdToString
        |> String.join " -> "
    )
        ++ " [ arrowhead=\"vee\" ]"


joinLines : List String -> String
joinLines =
    String.join "\n"


type alias GraphRenderConfiguration =
    { orientation : Orientation, showLegend : Bool }


toDOTString : GraphRenderConfiguration -> Graph ( Evaluation, NodeType ) -> String
toDOTString { orientation, showLegend } graph =
    let
        legendNode str evaluation =
            genericNode { id = "__" ++ str ++ "__", label = str, evaluation = evaluation, shape = "rect", fontSize = "10" }

        implicationTypeShape =
            case orientation of
                Horizontal ->
                    "rarrow"

                Vertical ->
                    "invtriangle"

        nodeToString : Node ( Evaluation, NodeType ) -> String
        nodeToString (Node nodeId ( text, ( evaluation, nodeType ) )) =
            genericNode
                { id = nodeIdToString nodeId
                , label = text
                , evaluation = evaluation
                , fontSize = "11"
                , shape =
                    case nodeType of
                        PropositionNode ->
                            "rect"

                        BooleanOperatorNode ->
                            "diamond"

                        ImplicationNode ->
                            implicationTypeShape

                        AssertionNode ->
                            "ellipse"
                }
    in
    joinLines
        [ "digraph {"
        , if not showLegend then
            ""

          else
            [ "subgraph clusterLegend {"
            , quotedProperties [ ( "label", "Legend" ), ( "bgcolor", "#e0e0ff" ) ]
            , legendNode "Unknown" EUnknown
            , legendNode "False" EFalse
            , legendNode "True" ETrue
            , legendNode "Contradiction" EContradiction
            , "}"
            ]
                |> joinLines
        , quotedProperties
            [ ( "rankdir"
              , case orientation of
                    Horizontal ->
                        "LR"

                    Vertical ->
                        "TB"
              )
            ]
        , "subgraph clusterMain {"
        , quotedProperties
            [ ( "bgcolor", "white" )
            ]
        , graph |> nodes |> List.map nodeToString |> joinLines
        , graph |> edges |> List.map edgeToString |> joinLines
        , "}"
        , "}"
        ]
