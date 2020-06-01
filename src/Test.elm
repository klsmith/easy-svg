module Test exposing (..)

import Color exposing (blue, green, red)
import Drawable exposing (..)
import EasySvg exposing (View)
import Html exposing (Html, pre, text)
import Shape exposing (circle)


main : Html msg
main =
    EasySvg.draw viewConfig
        [ testCircle
        , testCircle |> position 352 352
        ]


viewConfig : View
viewConfig =
    { x = 0
    , y = 0
    , width = 640
    , height = 640
    }


{-| outline doesn't render yet...
-}
testCircle : Drawable
testCircle =
    circle 240
        |> drawable
        |> position 320 320
        |> fill (Color.rgba 1 0 1 0.5)
        |> outline (Color.rgba 0 1 0 1) 32
