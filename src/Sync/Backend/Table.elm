module Sync.Backend.Table exposing (..)

import Dict exposing (Dict)
import Sync.Table exposing (..)
import Ulid exposing (Ulid(..))
import UlidSet exposing (UlidSet)
import IdSet exposing (IdSet)
import Random.NoOpaqueType as Random
import Id exposing (Id(..))

type Table user v
    = Table Random.Seed (Dict String (Value user v))

type alias Value user v =
    { value : Maybe v
    , writer : IdSet user
    , reader : IdSet user
    }

type alias NeedProcessing v =
    { response : ToFrontend v
    , changes : UlidSet v
    }


fromFrontend : (v -> v) -> Id user -> ToBackend v -> Table user v -> (Table user v, NeedProcessing v)
fromFrontend sanitize userId (ToBackend toBackend) myTable =
    let
        buildUp : (Ulid v, Id (Operation v), Operation v) -> (Table user v, NeedProcessing v) -> (Table user v, NeedProcessing v)
        buildUp (Ulid ulid, opId, op) (Table seed table, needProc) =
            case (Dict.get ulid table, op) of
                (Nothing, Create newVal) ->
                    let
                        sainVal = sanitize newVal
                    in
                    ( Dict.insert
                        ulid
                        { value = Just sainVal
                        , writer = IdSet.singleton userId
                        , reader = IdSet.empty
                        }
                        table
                        |> Table seed
                    , { response = insertToFront (Ulid ulid, Accepted opId, Create sainVal) needProc.response
                      , changes = UlidSet.insert (Ulid ulid) needProc.changes
                      }
                    )

                (Nothing, _) ->
                    noVal (Ulid ulid, opId, op) (Table seed table, needProc)

                (Just _, Create _) ->
                    let
                        (newFrontSeed, newTable) =
                            genSeed (Table seed table)

                        (ToFrontend toFrontend) =
                            needProc.response
                    in
                    ( newTable
                    , { needProc
                        | response =
                            { operations = (Ulid ulid, Rejected opId, op) :: toFrontend.operations
                            , newSeed = Just newFrontSeed
                            } |> ToFrontend
                        }
                    )

                (Just value, Update newVal) ->
                    if value.value == Nothing then
                        noVal (Ulid ulid, opId, op) (Table seed table, needProc)

                    else if IdSet.member userId value.writer then
                        let
                            sainVal = sanitize newVal
                        in
                        ( Dict.insert ulid { value | value = Just sainVal } table |> Table seed
                        , { response = insertToFront (Ulid ulid, Accepted opId, Update sainVal) needProc.response
                          , changes = UlidSet.insert (Ulid ulid) needProc.changes
                          }
                        )

                    else
                        ( Table seed table
                        , { needProc | response = insertToFront (Ulid ulid, Rejected opId, op) needProc.response }
                        )

                (Just value, Delete) ->
                    if value.value == Nothing then
                        noVal (Ulid ulid, opId, op) (Table seed table, needProc)

                    else if IdSet.member userId value.writer then
                        ( Dict.insert ulid { value = Nothing, writer = IdSet.empty, reader = IdSet.empty } table
                            |> Table seed
                        , { response = insertToFront (Ulid ulid, Accepted opId, Delete) needProc.response
                          , changes = UlidSet.insert (Ulid ulid) needProc.changes
                          }
                        )

                    else if IdSet.member userId value.reader then
                        ( Table seed table
                        , { needProc | response = insertToFront (Ulid ulid, Rejected opId, op) needProc.response }
                        )

                    else
                        noVal (Ulid ulid, opId, op) (Table seed table, needProc)

                (Just value, Read) ->
                    case value.value of
                        Nothing ->
                            noVal (Ulid ulid, opId, op) (Table seed table, needProc)

                        Just val ->
                            if IdSet.member userId value.reader || IdSet.member userId value.writer then
                                ( Table seed table
                                , { needProc | response = insertToFront (Ulid ulid, Accepted opId, Update val ) needProc.response }
                                )

                            else
                                noVal (Ulid ulid, opId, op) (Table seed table, needProc)

        noVal : (Ulid v, Id (Operation v), Operation v) -> (Table user v, NeedProcessing v) -> (Table user v, NeedProcessing v)
        noVal (Ulid ulid, opId, op) (Table seed table, needProc) =
            ( Table seed table
            , { needProc
                | response =
                    needProc.response
                    |> insertToFront (Ulid ulid, Rejected opId, op)
                    |> insertToFront (Ulid ulid, External, Delete)
                }
            )
    in
    List.foldl
        buildUp
        (myTable, { response = emptyToFront, changes = UlidSet.empty })
        (List.sortBy (\(_, Id opId, _) -> opId) toBackend.operations)


genSeed : Table user v -> (Random.Seed, Table user v)
genSeed (Table seed table) =
    Random.step (Random.int Random.minInt Random.maxInt) seed
        |> \(randInt, newSeed) ->
            ( Random.initialSeed randInt, Table newSeed table )

-- get : Id user -> Ulid v -> Table user v -> Maybe v
-- get userId (Ulid ulid) (Table _ table) =
--     Dict.get ulid table
--         |> Maybe.andThen (\{value, reader, writer} ->
--             if UlidSet.member userId reader || UlidSet.member userId writer then
--                 Just value
--             else
--                 Nothing
--         )

-- {-| Returns a bool showing if the user had auth to edit the value
--     Dont modify the table if the user does not have auth
-- -}
-- insert : Id user -> Ulid v -> v -> Table user v -> (Bool, Table user v, DistributeToFrontend user v)
-- insert userId (Ulid ulid) newVal (Table seed table) =
--     case Dict.get ulid table of
--         Nothing ->
--             ( True
--             , Dict.insert
--                 ulid
--                 { value = newVal
--                 , writer = UlidSet.singleton userId
--                 , reader = UlidSet.empty
--                 }
--                 table
--                 |> Table seed
--             , [( UlidSet.singleton userId, (Ulid ulid, External, Create newVal))]
--                 |> DistributeToFrontend
--             )

--         Just value ->
--             if UlidSet.member userId value.writer then
--                 ( True
--                 , Dict.insert ulid { value | value = newVal } table
--                     |> Table seed
--                 , [( UlidSet.union value.writer value.reader, (Ulid ulid, External, Update newVal))]
--                     |> DistributeToFrontend
--                 )

--             else
--                 ( False
--                 , Table seed table
--                 , DistributeToFrontend []
--                 )




-- type DistributeToFrontend user v
--     = DistributeToFrontend (List (IdSet user, (Ulid v, Validation v, Operation v)))

-- batchDistributeToFrontend : List (DistributeToFrontend user v) -> DistributeToFrontend user v
-- batchDistributeToFrontend =
--     List.foldl
--         (\(DistributeToFrontend list) acc ->
--             list ++ acc
--         )
--         []
--         >> DistributeToFrontend

-- distributeToFrontend : (Id user -> ToFrontend v -> Cmd msg) -> DistributeToFrontend user v -> Cmd msg
-- distributeToFrontend toCmd (DistributeToFrontend list) =
--     list
--         |> List.foldl
--             (\(userSet, detailedOp) runDict ->
--                 UlidSet.foldl
--                     (\userId runRunDict ->
--                         UlidDict.update userId
--                             (\maybeList ->
--                                 detailedOp :: (Maybe.withDefault [] maybeList)
--                                 |> Just
--                             )
--                             runRunDict
--                     )
--                     runDict
--                     userSet
--             )
--             UlidDict.empty
--         |> UlidDict.foldl
--             (\userId opList acc ->
--                 toCmd userId (ToFrontend (List.Extra.unique opList))
--                     :: acc
--             )
--             []
--         |> Cmd.batch

-- Internal

emptyToFront : ToFrontend v
emptyToFront =
    ToFrontend
        { newSeed = Nothing
        , operations = []
        }

insertToFront : (Ulid v, Validation v, Operation v) -> ToFrontend v -> ToFrontend v
insertToFront row (ToFrontend toFrontend) =
    ToFrontend {toFrontend | operations = row :: toFrontend.operations}
