port module Server.Ports exposing (..)

import Json.Encode exposing (Value)


-- WEBSERVER
-- DATABASE


port receive : (Value -> msg) -> Sub msg


port transact : Value -> Cmd msg
