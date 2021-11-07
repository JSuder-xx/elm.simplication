module Data.Graph exposing (Edge(..), Graph, Node(..), NodeId, edges, edgesFrom, empty, insertEdge, insertNode, insertNodes, nodeById, nodeId, nodes)

import Dict exposing (Dict)
import Html exposing (a)


{-| NodeId is opaque which provides most of the safety of this module as you cannot make a Node or Edge without a NodeId and you
cannot make a NodeId except by using insertNode.
-}
type NodeId
    = NodeId Int


nodeId : NodeId -> Int
nodeId (NodeId id) =
    id


type Node a
    = Node NodeId ( String, a )


type Edge
    = Edge NodeId NodeId


type Graph a
    = Graph
        { lastNodeId : NodeId
        , nodeDict : Dict Int (Node a)
        , edgeDict : Dict Int (List Edge)
        }


empty : Graph a
empty =
    Graph { lastNodeId = NodeId 0, nodeDict = Dict.empty, edgeDict = Dict.empty }


insertNode : ( String, a ) -> Graph a -> ( Graph a, NodeId )
insertNode ( str, a ) (Graph ({ lastNodeId, nodeDict } as graphRec)) =
    let
        (NodeId lastId) =
            lastNodeId

        newId =
            lastId + 1

        newNodeId =
            NodeId newId

        newNode =
            Node newNodeId ( str, a )
    in
    ( Graph
        { graphRec
            | lastNodeId = newNodeId
            , nodeDict = Dict.insert newId newNode nodeDict
        }
    , newNodeId
    )


insertNodes : List ( String, a ) -> Graph a -> ( Graph a, List NodeId )
insertNodes nodeTuples original =
    let
        fold node ( lastGraph, nodeIds ) =
            let
                ( newGraph, newNodeId ) =
                    insertNode node lastGraph
            in
            ( newGraph, newNodeId :: nodeIds )
    in
    nodeTuples
        |> List.foldl fold ( original, [] )
        |> Tuple.mapSecond List.reverse


nodes : Graph a -> List (Node a)
nodes (Graph { nodeDict }) =
    Dict.values nodeDict


nodeById : NodeId -> Graph a -> Maybe (Node a)
nodeById (NodeId id) (Graph { nodeDict }) =
    Dict.get id nodeDict


insertEdge : NodeId -> NodeId -> Graph a -> Graph a
insertEdge ((NodeId fromNode) as fromNodeId) toNodeId (Graph ({ edgeDict } as graphRecord)) =
    let
        newEdge =
            Edge fromNodeId toNodeId

        update edgesMaybe =
            case edgesMaybe of
                Nothing ->
                    Just [ newEdge ]

                Just existing ->
                    Just (newEdge :: existing)
    in
    Graph { graphRecord | edgeDict = Dict.update fromNode update edgeDict }


edges : Graph a -> List Edge
edges (Graph { edgeDict }) =
    Dict.values edgeDict |> List.concat


edgesFrom : NodeId -> Graph a -> List Edge
edgesFrom (NodeId id) (Graph { edgeDict }) =
    Dict.get id edgeDict |> Maybe.withDefault []
