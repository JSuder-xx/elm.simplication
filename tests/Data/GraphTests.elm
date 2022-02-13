module Data.GraphTests exposing (..)

import Data.Graph exposing (Node(..), empty, insertNode, insertNodes, nodeById, nodeId)
import Expect
import Fuzz exposing (..)
import List
import List.Extra
import Test exposing (..)
import Tuple


nodeLabel : Int -> String
nodeLabel n =
    "Node " ++ String.fromInt (9999 - n)


nodeData : Int -> ( String, String )
nodeData n =
    let
        s =
            nodeLabel n
    in
    ( s, s )


nodesInRange : Int -> Int -> List ( String, String )
nodesInRange lo hi =
    List.range lo hi |> List.map nodeData


addTo : List a -> a -> List a
addTo lst v =
    v :: lst


suite : Test
suite =
    describe "Graph"
        [ describe "insertNode" <|
            [ fuzz string "id of the first node is always 1" <|
                \s ->
                    let
                        ( _, id ) =
                            insertNode ( s, s ) empty
                    in
                    Expect.equal 1 (nodeId id)
            , fuzz (intRange 1 100) "inserting n nodes always yields monotonically increasing ids" <|
                \num ->
                    let
                        nums =
                            List.range 1 num

                        fold n ( lastGraph, ids ) =
                            insertNode (nodeData n) lastGraph |> Tuple.mapSecond (nodeId >> addTo ids)
                    in
                    Expect.equal nums (nums |> List.foldl fold ( empty, [] ) |> Tuple.second |> List.reverse)
            ]
        , describe "insertNodes" <|
            [ fuzz (intRange 1 50) "nodeIds are ever increasing" <|
                \num ->
                    Expect.equal
                        (insertNodes (nodesInRange 101 (100 + num)) empty |> Tuple.second |> List.map nodeId)
                        (List.range 1 num)
            ]
        , describe "nodeById" <|
            [ let
                ( base, count ) =
                    ( 100, 200 )
              in
              fuzz (intRange 0 (count - 1)) "always returns the appropriate node" <|
                \nth ->
                    let
                        ( graphWith100To130, nodeIds ) =
                            insertNodes (nodesInRange base (base + count - 1)) empty

                        nthNodeMaybe =
                            List.Extra.getAt nth nodeIds |> Maybe.andThen (\n -> nodeById n graphWith100To130)
                    in
                    case nthNodeMaybe of
                        Just (Node _ ( label, _ )) ->
                            Expect.equal label (nodeLabel (nth + base))

                        _ ->
                            Expect.fail "Expecting to find the node"
            ]

        -- , describe "insertEdge" <|
        --     case nodeIds of
        --         nodeOneId :: nodeTwoId :: nodeThreeId :: _ ->
        --             [ test "inserting a single edge from 1 and then getting edges from 1" <|
        --                 \_ -> Expect.equal 1 (empty |> insertEdge nodeOneId nodeTwoId |> edgesFrom nodeOneId |> List.length)
        --             , test "inserting a two edges from 1 and then getting edges from 1" <|
        --                 \_ -> Expect.equal 2 (empty |> insertEdge nodeOneId nodeTwoId |> insertEdge nodeOneId nodeThreeId |> edgesFrom nodeOneId |> List.length)
        --             , test "inserting a two edges from 1 and then getting edges from 2" <|
        --                 \_ -> Expect.equal 0 (empty |> insertEdge nodeOneId nodeTwoId |> insertEdge nodeOneId nodeThreeId |> edgesFrom nodeTwoId |> List.length)
        --             ]
        --         _ ->
        --             []
        ]
