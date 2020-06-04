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
        [ testCircle |> position 26 26
        , testCircle |> position 26 38
        , testCircle |> position 38 26
        , testCircle |> position 38 38
        , testRectangle |> position 32 32
        ]


testCircle : Drawable
testCircle =
    circle 24
        |> fill colorA
        |> outline colorB 3


testRectangle : Drawable
testRectangle =
    rectangle 16 16
        |> fill colorB
        |> outline colorA 2


colorA =
    Color.rgba 0.5 0.5 0.5 0.25


colorB =
    Color.rgba 0 0 0 0.5