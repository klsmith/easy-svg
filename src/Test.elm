module Test exposing (..)

import Color exposing (blue, green)
import Drawable exposing (..)
import EasySvg exposing (View)
import Html exposing (Html, pre, text)
import Shape exposing (circle)


main : Html msg
main =
    EasySvg.draw viewConfig [ testCircle ]


viewConfig : View
viewConfig =
    { x = 0
    , y = 0
    , width = 64
    , height = 64
    }


{-| outline doesn't render yet...
-}
testCircle : Drawable
testCircle =
    circle 32
        |> drawable
        |> position 32 32
        |> fill blue
        |> outline green 2
