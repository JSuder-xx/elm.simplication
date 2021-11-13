module Logic.AssertionGraph exposing (NodeType(..), fromAssertions)

import Data.Graph exposing (Graph, NodeId)
import Dict exposing (Dict)
import Logic.Assertion as A exposing (Assertion(..))
import Logic.BooleanExpression as BE exposing (BooleanExpression(..), PropositionTruth)
import Logic.Evaluation exposing (Evaluation(..))
import State as S


type NodeType
    = BooleanOperatorNode
    | ImplicationNode
    | PropositionNode
    | AssertionNode


type alias State =
    ( Dict String Evaluation, Graph ( Evaluation, NodeType ), Dict String NodeId )


type alias StateM result =
    S.State State result


insertPropositionIfNotExists : String -> StateM NodeId
insertPropositionIfNotExists propositionString =
    S.State
        (\(( symbolTable, graph, propositionDict ) as original) ->
            case Dict.get propositionString propositionDict of
                Just nodeId ->
                    ( nodeId, original )

                Nothing ->
                    let
                        ( newGraph, newNodeId ) =
                            Data.Graph.insertNode ( propositionString, ( BE.evaluate symbolTable (Proposition propositionString), PropositionNode ) ) graph
                    in
                    ( newNodeId, ( symbolTable, newGraph, Dict.insert propositionString newNodeId propositionDict ) )
        )


insertNodeType : NodeType -> String -> BooleanExpression -> StateM NodeId
insertNodeType nodeType operator expression =
    S.State <|
        \( symbolTable, graph, propositionDict ) ->
            let
                ( newGraph, newNodeId ) =
                    Data.Graph.insertNode ( operator, ( BE.evaluate symbolTable expression, nodeType ) ) graph
            in
            ( newNodeId, ( symbolTable, newGraph, propositionDict ) )


insertAssertion : BooleanExpression -> StateM NodeId
insertAssertion expr =
    insertNodeType AssertionNode "Assert" expr


insertOperator : String -> BooleanExpression -> StateM NodeId
insertOperator =
    insertNodeType BooleanOperatorNode


insertNot : BooleanExpression -> StateM NodeId
insertNot =
    insertOperator "Not"


insertThen : BooleanExpression -> StateM NodeId
insertThen =
    insertNodeType ImplicationNode "Then"


insertEdge : NodeId -> NodeId -> StateM ()
insertEdge fromNodeId toNodeId =
    S.modify <|
        \( symbolTable, graph, propositionDict ) ->
            ( symbolTable, Data.Graph.insertEdge fromNodeId toNodeId graph, propositionDict )


edgesFanIn : List NodeId -> NodeId -> StateM ()
edgesFanIn fromNodeIds toNodeId =
    S.foldlM (\_ -> \nodeId -> insertEdge nodeId toNodeId) () fromNodeIds |> toUnit


edgesFanOut : NodeId -> List NodeId -> StateM ()
edgesFanOut fromNodeId toNodeIds =
    S.foldlM (\_ -> insertEdge fromNodeId) () toNodeIds |> toUnit


insertExpressionIfNotExists : BooleanExpression -> StateM NodeId
insertExpressionIfNotExists e =
    let
        insertOperatorFromExpressions operatorStr innerExpressions =
            S.traverse insertExpressionIfNotExists innerExpressions
                |> S.andThen
                    (\innerNodeIds ->
                        insertOperator operatorStr e
                            |> S.andThen
                                (\operatorNodeId ->
                                    edgesFanIn innerNodeIds operatorNodeId |> S.map (\_ -> operatorNodeId)
                                )
                    )
    in
    case e of
        Proposition p ->
            insertPropositionIfNotExists p

        And innerExpressions ->
            insertOperatorFromExpressions "And" innerExpressions

        Not innerExpression ->
            insertOperatorFromExpressions "Not" [ innerExpression ]

        Or innerExpressions ->
            insertOperatorFromExpressions "Or" innerExpressions


insertPropositionTruth : PropositionTruth -> StateM NodeId
insertPropositionTruth ( proposition, truth ) =
    if truth then
        insertPropositionIfNotExists proposition

    else
        insertPropositionIfNotExists proposition
            |> S.andThen
                (\propositionNodeId ->
                    insertNot (Not (Proposition proposition))
                        |> S.andThen
                            (\notNodeId ->
                                insertEdge notNodeId propositionNodeId
                                    |> S.map (\_ -> notNodeId)
                            )
                )


toUnit : S.State s a -> S.State s ()
toUnit =
    S.map (\_ -> ())


assertionS : Assertion -> StateM ()
assertionS assertion =
    case assertion of
        AssertProposition p positive ->
            let
                expr =
                    Proposition p
                        |> (if positive then
                                identity

                            else
                                Not
                           )
            in
            insertAssertion expr
                |> S.andThen
                    (\assertNodeId ->
                        insertPropositionTruth ( p, positive )
                            |> S.andThen
                                (\propNodeId ->
                                    insertEdge assertNodeId propNodeId
                                )
                    )

        Implication expr consequents ->
            -- Expression node
            insertExpressionIfNotExists expr
                |> S.andThen
                    (\expressionNodeId ->
                        -- Then node
                        insertThen expr
                            |> S.andThen
                                (\thenNodeId ->
                                    -- Edge from Expression to "Then"
                                    insertEdge expressionNodeId thenNodeId
                                        |> S.andThen
                                            (\_ ->
                                                -- Consequent nodes
                                                S.traverse insertPropositionTruth consequents
                                                    |> S.andThen
                                                        (edgesFanOut thenNodeId)
                                            )
                                )
                    )


fromAssertions : List Assertion -> Graph ( Evaluation, NodeType )
fromAssertions assertions =
    let
        initialState : State
        initialState =
            ( A.applyAssertions Dict.empty assertions, Data.Graph.empty, Dict.empty )
    in
    S.finalState initialState (S.traverse assertionS assertions)
        |> (\( _, graph, _ ) -> graph)
