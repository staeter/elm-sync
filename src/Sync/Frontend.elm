module Sync.Frontend exposing
    ( Table(..), Value, Remote(..)
    , empty
    , add, insert, remove
    , get, optimisticGet, completeGet
    , isLoading
    , request
    , updateFromBackend
    )

import Random.NoOpaqueType as Random
import Time.NoOpaqueType as Time
import Sync exposing (..)
import Ulid exposing (Ulid)
import Id exposing (Id(..))
import IdDict exposing (IdTable)
import UlidDict exposing (UlidDict)


type Table k v
    = Table (UlidDict k (Value v))


type alias Value v =
    { remote : Remote v
    , local : IdTable (Operation v)
    }


type Remote v
    = Loading
    | Received v
    | RemoteDeleted


empty : Table k v
empty =
    Table UlidDict.empty


add :
    v
    -> Table k v
    -> Random.Seed
    -> Time.Posix
    -> { newValId : Ulid k
       , newTable : Table k v
       , toBackend : ToBackend k v
       , newSeed : Random.Seed
       }
add val (Table table) seed now =
    let
        (ulid, newSeed) =
            Ulid.new seed now

        (opId, newLocal) =
            IdDict.add (Create val) IdDict.empty

        newValue =
            { local = newLocal
            , remote = Loading
            }
    in
    { newValId = ulid
    , newTable = UlidDict.insert ulid newValue table |> Table
    , toBackend = toBack (ulid, opId, Create val)
    , newSeed = newSeed
    }


insert : Ulid k -> v -> Table k v -> { newTable : Table k v, toBackend : ToBackend k v }
insert ulid newVal (Table table) =
    case UlidDict.get ulid table of
        Nothing ->
            let
                (opId, newLocal) =
                    IdDict.add (Create newVal) IdDict.empty
            in
            { newTable = UlidDict.insert ulid { local = newLocal, remote = Loading } table |> Table
            , toBackend = toBack (ulid, opId, Create newVal)
            }

        Just value ->
            if pendingOp value == Just (Update newVal) then
                { newTable = Table table, toBackend = emptyToBack }
            else
                let
                    (opId, newLocal) =
                        IdDict.add (Update newVal) value.local
                in
                { newTable = UlidDict.insert ulid { value | local = newLocal } table |> Table
                , toBackend = toBack (ulid, opId, Update newVal)
                }


remove : Ulid k -> Table k v -> { newTable : Table k v, toBackend : ToBackend k v }
remove ulid (Table table) =
    case UlidDict.get ulid table of
        Nothing ->
            let
                (opId, newLocal) =
                    IdDict.add Delete IdDict.empty
            in
            { newTable = UlidDict.insert ulid { local = newLocal, remote = Loading } table |> Table
            , toBackend = toBack (ulid, opId, Delete)
            }

        Just value ->
            if pendingOp value == Just Delete then
                { newTable = Table table, toBackend = emptyToBack }

            else
                let
                    (opId, newLocal) =
                        IdDict.add Delete value.local
                in
                { newTable = UlidDict.insert ulid { value | local = newLocal } table |> Table
                , toBackend = toBack (ulid, opId, Delete)
                }


get : Ulid k -> Table k v -> Maybe v
get ulid (Table table) =
    UlidDict.get ulid table
        |> Maybe.andThen (\value ->
            case value.remote of
                Received val ->
                    Just val

                _ ->
                    Nothing
        )


isLoading : Ulid k -> Table k v -> Bool
isLoading ulid (Table table) =
    UlidDict.get ulid table
        |> Maybe.map
            (\value ->
                value.remote == Loading || not (IdDict.isEmpty value.local)
            )
        |> Maybe.withDefault False


optimisticGet : Ulid k -> Table k v -> Maybe v
optimisticGet ulid (Table table) =
    Maybe.andThen
        (\value ->
            case value.remote of
                Received val ->
                    Just val

                _ ->
                    IdDict.highestId value.local
                        |> Maybe.andThen (\(_, op) ->
                            case op of
                                Create val ->
                                    Just val

                                Update val ->
                                    Just val

                                Delete ->
                                    Nothing

                                Read ->
                                    Nothing
                        )
        )
        (UlidDict.get ulid table)


completeGet : Ulid k -> Table k v -> Maybe (Value v)
completeGet ulid (Table table) =
    UlidDict.get ulid table


{-| Pull the value from the server if it isn't already present.
-}
request : Ulid k -> Table k v -> { newTable : Table k v, toBackend : ToBackend k v }
request ulid (Table table) =
    case UlidDict.get ulid table of
        Just _ ->
            { newTable = Table table, toBackend = emptyToBack }

        Nothing ->
            let
                (opId, newLocal) =
                    IdDict.add Read IdDict.empty
            in
            { newTable = UlidDict.insert ulid { local = newLocal, remote = Loading } table |> Table
            , toBackend = toBack (ulid, opId, Read)
            }


updateFromBackend : ToFrontend k v -> Table k v -> Table k v
updateFromBackend (ToFrontend operations) (Table table) =
    let
        updateNonExisting valid op =
            case (valid, op) of
                (External, Create val) ->
                    { local = IdDict.empty, remote = Received val } |> Just

                _ ->
                    Nothing

        updateExisting valid op value =
            case (valid, op) of
                (Rejected opId, _) ->
                    let
                        newLocal = IdDict.remove opId value.local
                    in
                    if value.remote == RemoteDeleted && IdDict.isEmpty newLocal then
                        Nothing

                    else
                        { value | local = IdDict.remove opId value.local } |> Just

                (Accepted opId, Create val) ->
                    { local = IdDict.remove opId value.local
                    , remote = Received val
                    } |> Just

                (Accepted opId, Update val) ->
                    { local = IdDict.remove opId value.local
                    , remote = Received val
                    } |> Just

                (Accepted opId, Delete) ->
                    let
                        newLocal = IdDict.remove opId value.local
                    in
                    if IdDict.isEmpty newLocal then
                        Nothing

                    else
                        { local = newLocal
                        , remote = RemoteDeleted
                        } |> Just

                (External, Create val) ->
                    { value | remote = Received val } |> Just

                (External, Update val) ->
                    { value | remote = Received val } |> Just

                (External, Delete) ->
                    { value | remote = RemoteDeleted } |> Just

                (Accepted opId, Read) ->
                    -- This should never happen since the server responds to reads with updates
                    { value | local = IdDict.remove opId value.local } |> Just

                (External, Read) ->
                    Just value
    in
    List.foldl
        (\(ulid, valid, op) runTable ->
            UlidDict.update
                ulid
                ( Maybe.map (updateExisting valid op)
                >> Maybe.withDefault (updateNonExisting valid op)
                )
                runTable
        )
        table
        operations
        |> Table


-- Internal


pendingOp : Value v -> Maybe (Operation v)
pendingOp value =
    IdDict.highestId value.local
    |> Maybe.map Tuple.second


toBack : (Ulid k, Id (Operation v), Operation v) -> ToBackend k v
toBack (ulid, opId, op) =
    [(ulid, opId, op)] |> ToBackend


emptyToBack : ToBackend k v
emptyToBack =
    ToBackend []
