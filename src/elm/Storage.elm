port module Storage exposing (..)

import Models exposing (..)


-- PORTS


port saveToLocalStorage : List SaveableTask -> Cmd msg


port loadFromLocalStorage : () -> Cmd msg


port localStorageSubscription : (List SaveableTask -> msg) -> Sub msg
