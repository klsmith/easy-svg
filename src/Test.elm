module Test exposing (..)

import Color exposing (blue, green, red)
import Drawable exposing (..)
import EasySvg exposing (Camera, PortView)
import Html exposing (Html, pre, text)


main : Html msg
main =
    EasySvg.draw
        (PortView 640 640)
        (Camera 0 0 64 64)
        [ testCircle |> position 27 27
        , testCircle |> position 38 38
        ]


testCircle : Drawable
testCircle =
    circle 24
        |> fill colorA
        |> outline colorB 3


colorA =
    Color.rgba 1 0 1 0.5


colorB =
    Color.rgba 0 1 0 1
