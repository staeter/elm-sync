module Sync exposing (..)

import Ulid exposing (Ulid)
import Id exposing (Id)


type Operation v
    = Read
    | Create v
    | Update v
    | Delete


type ToBackend k v
    = ToBackend (List (Ulid k, Id (Operation v), Operation v))


type ToFrontend k v
    = ToFrontend (List (Ulid k, Validation v, Operation v))


type Validation v
    = Accepted (Id (Operation v))
    | Rejected (Id (Operation v))
    | External


batchToBackend : List (ToBackend k v) -> ToBackend k v
batchToBackend =
    List.foldl
        (\(ToBackend operations) acc ->
            operations ++ acc
        )
        []
        >> ToBackend


batchToFrontend : List (ToFrontend k v) -> ToFrontend k v
batchToFrontend =
    List.foldl
        (\(ToFrontend operations) acc ->
            operations ++ acc
        )
        []
        >> ToFrontend


-- lamdera

type alias ClientId = String
type alias SessionId = String
