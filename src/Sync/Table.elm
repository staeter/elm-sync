module Sync.Table exposing (..)

import Ulid exposing (Ulid)
import Id exposing (Id)
import Random.NoOpaqueType as Random
import Maybe.Extra


type Operation v
    = Read
    | Create v
    | Update v
    | Delete


type ToBackend v
    = ToBackend
        { requestSeed : Bool
        , operations : List (Ulid v, Id (Operation v), Operation v)
        }


type ToFrontend v
    = ToFrontend
        { newSeed : Maybe Random.Seed
        , operations : List (Ulid v, Validation v, Operation v)
        }


type Validation v
    = Accepted (Id (Operation v))
    | Rejected (Id (Operation v))
    | External


batchToBackend : List (ToBackend v) -> ToBackend v
batchToBackend =
    List.foldl
        (\(ToBackend {requestSeed, operations}) acc ->
            { requestSeed = requestSeed || acc.requestSeed
            , operations = operations ++ acc.operations
            }
        )
        { requestSeed = False, operations = []}
        >> ToBackend


batchToFrontend : List (ToFrontend v) -> ToFrontend v
batchToFrontend =
    List.foldl
        (\(ToFrontend {newSeed, operations}) acc ->
            { newSeed = Maybe.Extra.or newSeed acc.newSeed
            , operations = operations ++ acc.operations
            }
        )
        { newSeed = Nothing, operations = []}
        >> ToFrontend


-- lamdera

type alias ClientId = String
type alias SessionId = String
