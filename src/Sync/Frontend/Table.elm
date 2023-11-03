module Sync.Frontend.Table exposing
    ( Table(..), Value, Remote(..)
    , add, insert, remove
    , get, optimisticGet, completeGet
    , isLoading
    , request
    , fromBackend
    )

import Dict exposing (Dict)
import Random.NoOpaqueType as Random
import Time.NoOpaqueType as Time
import Ulid exposing (Ulid(..))
import Sync.Table exposing (..)
import Ulid exposing (Ulid(..))
import Dict
import Id exposing (Id(..))
import IdDict exposing (IdTable)

type Table v
    = Table Random.Seed (Dict String (Value v))

type alias Value v =
    { remote : Remote v
    , local : IdTable (Operation v)
    }

type Remote v
    = Loading
    | Received v
    | RemoteDeleted


add : v -> Table v -> Time.Posix -> ( Ulid v, Table v, ToBackend v )
add val (Table seed table) now =
    let
        (ulid, newSeed) =
            Random.step (Ulid.stringGenerator now) seed
    in
    if Dict.member ulid table then
        -- this should never happen but we secure against ulid collision anyway
        if (Dict.get ulid table |> Maybe.andThen pendingOp) == Just (Create val) then
            (Ulid ulid, Table newSeed table, emptyToBack)

        else
            -- TODO track when this happens and report it, it should come from an issue in the code
            add val (Table newSeed table) now
            |> (\(ulid_, table_, ToBackend toBackend) ->
                (ulid_, table_, ToBackend { toBackend | requestSeed = True })
            )

    else
        let
            (opId, newLocal) =
                IdDict.add (Create val) IdDict.empty

            newValue =
                { local = newLocal
                , remote = Loading
                }
        in
        ( Ulid ulid
        , Dict.insert ulid newValue table |> Table newSeed
        , toBack (Ulid ulid, opId, Create val)
        )


insert : Ulid v -> v -> Table v -> (Table v, ToBackend v)
insert (Ulid ulid) val (Table seed table) =
    case Dict.get ulid table of
        Nothing ->
            let
                (opId, newLocal) =
                    IdDict.add (Create val) IdDict.empty
            in
            ( Dict.insert ulid { local = newLocal, remote = Loading } table
                |> Table seed
            , toBack (Ulid ulid, opId, Create val)
            )

        Just value ->
            if pendingOp value == Just (Update val) then
                (Table seed table, emptyToBack)
            else
                let
                    (opId, newLocal) =
                        IdDict.add (Update val) value.local
                in
                ( Dict.insert ulid { value | local = newLocal } table
                    |> Table seed
                , toBack (Ulid ulid, opId, Update val)
                )


remove : Ulid v -> Table v -> (Table v, ToBackend v)
remove (Ulid ulid) (Table seed table) =
    case Dict.get ulid table of
        Nothing ->
            let
                (opId, newLocal) =
                    IdDict.add Delete IdDict.empty
            in
            ( Dict.insert ulid { local = newLocal, remote = Loading } table
                |> Table seed
            , toBack (Ulid ulid, opId, Delete)
            )

        Just value ->
            if pendingOp value == Just Delete then
                (Table seed table, emptyToBack)

            else
                let
                    (opId, newLocal) =
                        IdDict.add Delete value.local
                in
                ( Dict.insert ulid { value | local = newLocal } table
                    |> Table seed
                , toBack (Ulid ulid, opId, Delete)
                )


get : Ulid v -> Table v -> Maybe v
get (Ulid ulid) (Table _ table) =
    Dict.get ulid table
        |> Maybe.andThen (\value ->
            case value.remote of
                Received val ->
                    Just val

                _ ->
                    Nothing
        )


isLoading : Ulid v -> Table v -> Bool
isLoading (Ulid ulid) (Table _ table) =
    Dict.get ulid table
        |> Maybe.map (\value ->
            value.remote == Loading
            || not (IdDict.isEmpty value.local)
        )
        |> Maybe.withDefault False


optimisticGet : Ulid v -> Table v -> Maybe v
optimisticGet (Ulid ulid) (Table _ table) =
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
        (Dict.get ulid table)


completeGet : Ulid v -> Table v -> Maybe (Value v)
completeGet (Ulid ulid) (Table _ table) =
    Dict.get ulid table


{-| Pull the value from the server if it isn't already present.
-}
request : Ulid v -> Table v -> (Table v, ToBackend v)
request (Ulid ulid) (Table seed table) =
    case Dict.get ulid table of
        Just _ ->
            (Table seed table, emptyToBack)

        Nothing ->
            let
                (opId, newLocal) =
                    IdDict.add Read IdDict.empty
            in
            ( Dict.insert ulid { local = newLocal, remote = Loading } table
                |> Table seed
            , toBack (Ulid ulid, opId, Read)
            )


fromBackend : ToFrontend v -> Table v -> Table v
fromBackend (ToFrontend toFrontend) (Table seed table) =
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
        (\(Ulid ulid, valid, op) runTable ->
            Dict.update
                ulid
                ( Maybe.map (updateExisting valid op)
                >> Maybe.withDefault (updateNonExisting valid op)
                )
                runTable
        )
        table
        toFrontend.operations
        |> Table (Maybe.withDefault seed toFrontend.newSeed)


-- Internal


pendingOp : Value v -> Maybe (Operation v)
pendingOp value =
    IdDict.highestId value.local
    |> Maybe.map Tuple.second


toBack : (Ulid v, Id (Operation v), Operation v) -> ToBackend v
toBack (ulid, opId, op) =
    { operations = [(ulid, opId, op)], requestSeed = False }
    |> ToBackend


emptyToBack : ToBackend v
emptyToBack =
    { operations = [], requestSeed = False }
    |> ToBackend
