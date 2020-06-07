module Test exposing (main)

import Color exposing (Color, green, red)
import Drawable exposing (..)
import EasySvg exposing (Camera, PortView)
import Element as E
import Element.Background as EBackground
import Element.Border as EBorder
import Html exposing (Html)


{-| Don't forget to remove elm-ui dependency whenever the library gets finalized!
-}
main : Html msg
main =
    E.layout [ EBackground.color (E.rgba 0 0 0 1) ]
        (E.el
            [ E.centerX
            , E.centerY
            , EBorder.color (E.rgba 1 0 0 1)
            , EBorder.width 1
            ]
            (E.html
                (EasySvg.draw
                    portView
                    camera
                    scene
                )
            )
        )


portView : PortView
portView =
    { width = 12 * 64
    , height = 9 * 64
    }


camera : Camera
camera =
    { centerX = 0
    , centerY = 0
    , width = portView.width / 8
    , height = portView.height / 8
    }


background : Drawable
background =
    rectangle camera.width camera.height
        |> translate camera.centerX camera.centerY


scene : List Drawable
scene =
    [ background |> fill white
    , testCircleGroup
        |> scale 0.5
        |> skewX 400
        |> skewY 20
    , testRectangle
    , ellipse 5 10
        |> fill green
    , triangle 6
        |> fill red
        |> outline green 1
        |> translate 16 -16
        |> rotate 45
    , stopSign
        |> translate -16 16
        |> rotate -45
    , alsoStopSign
        |> translate 16 16
        |> rotate 45
    , arrow
        |> fill black
        |> rotate 45
        |> translate -32 -12
    ]


arrow : Drawable
arrow =
    polygon
        [ ( 12, 0 )
        , ( -6, 6 )
        , ( 0, 0 )
        , ( -6, -6 )
        ]


stopSign : Drawable
stopSign =
    let
        baseOct =
            octagon 6 |> rotate 22
    in
    group
        [ baseOct
            |> fill red
        , baseOct
            |> scale 0.9
            |> outline white 0.25
        , text impactFont 4.5 "STOP"
            |> fill white
        ]


alsoStopSign : Drawable
alsoStopSign =
    image 16 16 "https://lh3.googleusercontent.com/proxy/M5_6UggNSMQ9zIdPgfGcjjBStCWCnSlrL-jkFiz3YGO5zjP07q-oR8361SLjx7XnLuljVyBSahqLgnQuvw"


impactFont : FontFamily
impactFont =
    Multiple [ "Impact", "Charcoal", "sans-serif" ]


testCircleGroup : Drawable
testCircleGroup =
    let
        testCircle =
            circle 24
                |> fill colorA
                |> outline colorB 3
    in
    group
        [ testCircle |> translate -6 -6
        , testCircle |> translate -6 6
        , testCircle |> translate 6 -6
        , testCircle |> translate 6 6
        ]


testRectangle : Drawable
testRectangle =
    rectangle 16 16
        |> fill colorB
        |> outline colorA 2
        |> rotate 45
        |> scaleX 2


black : Color
black =
    Color.rgba 0 0 0 1


violet : Color
violet =
    Color.rgba 1 0 1 1


yellow : Color
yellow =
    Color.rgba 1 1 0 1


white : Color
white =
    Color.rgba 1 1 1 1


red : Color
red =
    Color.rgba 1 0 0 1


green : Color
green =
    Color.rgba 0 1 0 0.6


colorA : Color
colorA =
    Color.rgba 0.5 0.5 0.5 0.25


colorB : Color
colorB =
    Color.rgba 0 0 0 0.5


altScene : List Drawable
altScene =
    [ circle (camera.width / 6)
        |> fill red
        |> translate (camera.width / 8) (camera.height / 16)
    , circle (camera.width / 11)
        |> fill yellow
        |> translate (-camera.width / 3) (camera.height / 4)
    , circle (camera.width / 24)
        |> fill violet
        |> translate (-camera.width / 4) (-camera.height / 4)
    , ellipse (camera.width / 8) (camera.height / 16)
        |> fill colorA
        |> translate (camera.width / 3) (-camera.height / 3)
    ]
