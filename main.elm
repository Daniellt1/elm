module Main exposing (..)

import Html exposing (Html)
import State exposing (..)
import Subscriptions exposing (..)
import Types exposing (..)
import View exposing (..)


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }
