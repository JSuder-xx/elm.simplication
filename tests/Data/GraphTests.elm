module Data.GraphTests exposing (..)

import Data.Graph exposing (Node(..), edgesFrom, empty, insertEdge, insertNode, insertNodes, nodeId, nodes)
import Debug exposing (toString)
import Expect
import Test exposing (..)


suite : Test
suite =
    let
        nodeValues =
            List.range 100 104 |> List.map (\n -> ( toString n, "?" ))

        ( graphWith100To104, nodeIds ) =
            insertNodes nodeValues empty
    in
    describe "Graph"
        [ describe "insertNode" <|
            let
                ( graphWithFirst, firstNodeId ) =
                    insertNode ( "Hello", "a" ) empty

                ( graphWithSecond, secondNodeId ) =
                    insertNode ( "Goodbye", "a" ) graphWithFirst
            in
            [ test "id of first node is 1" <| \_ -> Expect.equal 1 (nodeId firstNodeId)
            , test "id of second node is 2" <| \_ -> Expect.equal 2 (nodeId secondNodeId)
            , test "graphWithSecond has two nodes" <| \_ -> Expect.equal 2 (graphWithSecond |> nodes |> List.length)
            ]
        , describe "insertNodes" <|
            [ test "nodeIds are in the correct order" <| \_ -> Expect.equal [ 1, 2, 3, 4, 5 ] (nodeIds |> List.map nodeId)
            , test "graph contains the length of nodes inserted" <| \_ -> Expect.equal 5 (graphWith100To104 |> nodes |> List.length)
            , test "graph contains the nodes inserted" <| \_ -> Expect.equal [ "100", "101", "102", "103", "104" ] (graphWith100To104 |> nodes |> List.map (\(Node _ ( str, _ )) -> str))
            ]
        , describe "insertEdge" <|
            case nodeIds of
                nodeOneId :: nodeTwoId :: nodeThreeId :: _ ->
                    [ test "inserting a single edge from 1 and then getting edges from 1" <|
                        \_ -> Expect.equal 1 (empty |> insertEdge nodeOneId nodeTwoId |> edgesFrom nodeOneId |> List.length)
                    , test "inserting a two edges from 1 and then getting edges from 1" <|
                        \_ -> Expect.equal 2 (empty |> insertEdge nodeOneId nodeTwoId |> insertEdge nodeOneId nodeThreeId |> edgesFrom nodeOneId |> List.length)
                    , test "inserting a two edges from 1 and then getting edges from 2" <|
                        \_ -> Expect.equal 0 (empty |> insertEdge nodeOneId nodeTwoId |> insertEdge nodeOneId nodeThreeId |> edgesFrom nodeTwoId |> List.length)
                    ]

                _ ->
                    []
        ]
