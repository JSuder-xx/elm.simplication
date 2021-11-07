module Logic.AssertionGraphTest exposing (..)

import Data.Graph exposing (Node(..), edges, nodes)
import Expect exposing (..)
import Logic.Assertion exposing (Assertion(..))
import Logic.AssertionGraph exposing (NodeType(..), fromAssertions)
import Logic.BooleanExpression exposing (BooleanExpression(..))
import Logic.Evaluation exposing (Evaluation(..))
import Test exposing (..)


op : a -> b -> ( a, ( b, NodeType ) )
op str eval =
    ( str, ( eval, BooleanOperatorNode ) )


prop : a -> b -> ( a, ( b, NodeType ) )
prop str eval =
    ( str, ( eval, PropositionNode ) )


thenOp : a -> ( String, ( a, NodeType ) )
thenOp eval =
    ( "Then", ( eval, ImplicationNode ) )


notOp : b -> ( String, ( b, NodeType ) )
notOp =
    op "Not"


aProp : b -> ( String, ( b, NodeType ) )
aProp =
    prop "a"


bProp : b -> ( String, ( b, NodeType ) )
bProp =
    prop "b"


suite : Test
suite =
    describe "AssertionGraph.fromAssertions" <|
        let
            expectNodes assertions expectedNodes _ =
                let
                    actualNodes =
                        fromAssertions assertions |> nodes |> List.map (\(Node _ t) -> t)
                in
                Expect.equal expectedNodes actualNodes

            expectEdges assertions expectedEdges _ =
                let
                    actualEdges =
                        fromAssertions assertions |> edges
                in
                Expect.equal expectedEdges actualEdges
        in
        [ test "a single proposition yields one node" <|
            expectNodes [ TruthOfProposition "x" True ] [ ( "x", ( ETrue, PropositionNode ) ) ]
        , test "a single proposition yields no edges" <|
            expectEdges [ TruthOfProposition "x" True ] []
        , test "an implication from a to b yields three nodes 'a', 'then', 'b' of unknown" <|
            expectNodes [ Implication (Proposition "a") [ ( "b", True ) ] ] [ aProp EUnknown, thenOp EUnknown, bProp EUnknown ]
        , test "an implication from a to b and a stated to be True yields two nodes of ETrue" <|
            expectNodes
                [ TruthOfProposition "a" True
                , Implication (Proposition "a") [ ( "b", True ) ]
                ]
                [ aProp ETrue, thenOp ETrue, bProp ETrue ]
        , test "an implication from not a to b and a stated to be True" <|
            expectNodes
                [ TruthOfProposition "a" True
                , Implication (Not <| Proposition "a") [ ( "b", True ) ]
                ]
                [ aProp ETrue, notOp EFalse, thenOp EFalse, bProp EUnknown ]
        , test "an implication from not a to b and a stated to be False" <|
            expectNodes
                [ TruthOfProposition "a" False
                , Implication (Not <| Proposition "a") [ ( "b", True ) ]
                ]
                [ aProp EFalse, notOp ETrue, thenOp ETrue, bProp ETrue ]
        , test "an implication from a to not a and a stated to be true" <|
            expectNodes
                [ TruthOfProposition "a" True
                , Implication (Proposition "a") [ ( "a", False ) ]
                ]
                [ aProp EContradiction, thenOp EContradiction, notOp EContradiction ]
        , test "an implication from a to not b and a stated to be True yields four nodes" <|
            expectNodes
                [ TruthOfProposition "a" True
                , Implication (Proposition "a") [ ( "b", False ) ]
                ]
                [ aProp ETrue, thenOp ETrue, bProp EFalse, notOp ETrue ]
        ]
