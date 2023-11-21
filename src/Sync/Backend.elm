module Sync.Backend exposing (..)

import Sync exposing (..)
import Ulid exposing (Ulid(..))
import UlidSet exposing (UlidSet)
import UlidDict exposing (UlidDict)
import Random.NoOpaqueType as Random
import Id exposing (Id(..))


type Table user k v
    = Table (UlidDict k (Value user v))


type alias Value user v =
    { value : Maybe v
    , owner : Auth user
    , writers : UlidSet user
    , readers : Access user
    }


type Access user
    = Private (UlidSet user)
    | Public


type alias NeedProcessing user k v =
    { response : ToFrontend k v
    , addToListening : UlidSet k
    , changes : UlidSet k
    , newTable : Table user k v
    }


type Auth user
    = User (Ulid user)
    | Admin


empty : Table user k v
empty =
    Table UlidDict.empty


updateFromFrontend : (v -> v) -> Auth user -> ToBackend k v -> Table user k v -> NeedProcessing user k v
updateFromFrontend sanitize auth (ToBackend toBackend) initTable =
    let
        buildUp : (Ulid k, Id (Operation v), Operation v) -> NeedProcessing user k v -> NeedProcessing user k v
        buildUp (ulid, opId, op) needProc =
            let
                (Table table) =
                    needProc.newTable
            in
            case (UlidDict.get ulid table, op) of
                (Nothing, Create newVal) ->
                    let
                        sainVal = sanitize newVal
                    in
                    { response = insertToFront (ulid, Accepted opId, Create sainVal) needProc.response
                    , changes = UlidSet.insert ulid needProc.changes
                    , addToListening = UlidSet.insert ulid needProc.addToListening
                    , newTable =
                        UlidDict.insert
                            ulid
                            { value = Just sainVal
                            , owner = auth
                            , writers = UlidSet.empty
                            , readers = Private UlidSet.empty
                            }
                            table
                            |> Table
                    }

                (Nothing, _) ->
                    noVal (ulid, opId, op) needProc

                (Just _, Create _) ->
                    let
                        (ToFrontend operations) =
                            needProc.response
                    in
                    { needProc
                    | response = (ulid, Rejected opId, op) :: operations |> ToFrontend
                    }

                (Just value, Update newVal) ->
                    if value.value == Nothing then
                        noVal (ulid, opId, op) needProc

                    else if hasWriteAccess auth value then
                        let
                            sainVal = sanitize newVal
                        in
                        { response = insertToFront (ulid, Accepted opId, Update sainVal) needProc.response
                        , changes = UlidSet.insert ulid needProc.changes
                        , addToListening = UlidSet.insert ulid needProc.addToListening
                        , newTable = UlidDict.insert ulid { value | value = Just sainVal } table |> Table
                        }

                    else
                        { needProc | response = insertToFront (ulid, Rejected opId, op) needProc.response }

                (Just value, Delete) ->
                    if value.value == Nothing then
                        noVal (ulid, opId, op) needProc

                    else if hasWriteAccess auth value then
                        { response = insertToFront (ulid, Accepted opId, Delete) needProc.response
                        , changes = UlidSet.insert ulid needProc.changes
                        , addToListening = UlidSet.insert ulid needProc.addToListening
                        , newTable =
                            UlidDict.insert
                                ulid
                                { value | value = Nothing, writers = UlidSet.empty, readers = Private UlidSet.empty }
                                table
                            |> Table
                        }

                    else
                        noVal (ulid, opId, op) needProc

                (Just value, Read) ->
                    case value.value of
                        Nothing ->
                            noVal (ulid, opId, op) needProc

                        Just val ->
                            if hasReadAccess auth value then
                                { needProc
                                | response = insertToFront (ulid, Accepted opId, Update val ) needProc.response
                                , addToListening = UlidSet.insert ulid needProc.addToListening
                                }

                            else
                                noVal (ulid, opId, op) needProc

        noVal : (Ulid k, Id (Operation v), Operation v) -> NeedProcessing user k v -> NeedProcessing user k v
        noVal (ulid, opId, op) needProc =
            { needProc
            | response =
                needProc.response
                |> insertToFront (ulid, Rejected opId, op)
                |> insertToFront (ulid, External, Delete)
            }
    in
    List.foldl
        buildUp
        { response = emptyToFront
        , addToListening = UlidSet.empty
        , changes = UlidSet.empty
        , newTable = initTable
        }
        (List.sortBy (\(_, Id opId, _) -> opId) toBackend)


hasReadAccess : Auth user -> Value user v -> Bool
hasReadAccess auth value =
    let
        isReader userId =
            case value.readers of
                Public -> True
                Private readSet ->
                    UlidSet.member userId readSet
    in
    case auth of
        Admin ->
            True

        User userId ->
            isReader userId || hasWriteAccess auth value


hasWriteAccess : Auth user -> Value user v -> Bool
hasWriteAccess auth { owner, writers } =
    case auth of
        Admin ->
            True

        User userId ->
            auth == owner || UlidSet.member userId writers


forwardChanges : Auth user -> UlidSet k -> NeedProcessing user k v -> Table user k v -> Maybe (ToFrontend k v)
forwardChanges userId listening {changes} table =
    let
        operations =
            UlidSet.foldl
                (\listeningId opAcc->
                    if not (UlidSet.member listeningId changes) then
                        opAcc
                    else
                        case get userId listeningId table of
                            Nothing ->
                                (listeningId, External, Delete ) :: opAcc

                            Just val ->
                                (listeningId, External, Update val ) :: opAcc
                )
                []
                listening
    in
    if List.isEmpty operations then
        Nothing

    else
        operations |> ToFrontend |> Just


get : Auth user -> Ulid k -> Table user k v -> Maybe v
get userId (ulid) (Table table) =
    UlidDict.get ulid table
        |> Maybe.andThen (\value ->
            if hasReadAccess userId value then
                value.value
            else
                Nothing
        )


find : Auth user -> (Ulid k -> v -> Bool) -> Table user k v -> Maybe (Ulid k, v)
find userId predicate (Table table) =
    UlidDict.foldl
        (\ulid value acc ->
            case (acc, value.value) of
                (Just _, _) ->
                    acc

                (Nothing, Just val) ->
                    if hasReadAccess userId value && predicate ulid val then
                        Just (ulid, val)
                    else
                        Nothing

                (Nothing, Nothing) ->
                    Nothing
        )
        Nothing
        table


-- Internal

emptyToFront : ToFrontend k v
emptyToFront =
    ToFrontend []

insertToFront : (Ulid k, Validation v, Operation v) -> ToFrontend k v -> ToFrontend k v
insertToFront row (ToFrontend operations) =
    ToFrontend (row :: operations)
