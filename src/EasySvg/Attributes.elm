module EasySvg.Attributes exposing (..)

import Color exposing (Color)
import Svg
import Svg.Attributes


viewBox : Float -> Float -> Float -> Float -> Svg.Attribute msg
viewBox x y w h =
    [ x, y, w, h ]
        |> List.map String.fromFloat
        |> String.join " "
        |> Svg.Attributes.viewBox


width : Float -> Svg.Attribute msg
width =
    Svg.Attributes.width << String.fromFloat


height : Float -> Svg.Attribute msg
height =
    Svg.Attributes.height << String.fromFloat


r : Float -> Svg.Attribute msg
r =
    Svg.Attributes.r << String.fromFloat


position : ( Float, Float ) -> List (Svg.Attribute msg)
position ( x, y ) =
    [ cx x, cy y ]


cx : Float -> Svg.Attribute msg
cx =
    Svg.Attributes.cx << String.fromFloat


cy : Float -> Svg.Attribute msg
cy =
    Svg.Attributes.cy << String.fromFloat



-- COLOR


maybeOutline : Maybe ( Color, Float ) -> List (Svg.Attribute msg)
maybeOutline maybeValue =
    case maybeValue of
        Just ( color, sw ) ->
            [ stroke color
            , strokeOpacityFromColor color
            , strokeWidth sw
            ]

        Nothing ->
            [ strokeOpacity 0 ]


stroke : Color -> Svg.Attribute msg
stroke color =
    Svg.Attributes.stroke (renderColor color)


strokeWidth : Float -> Svg.Attribute msg
strokeWidth sw =
    Svg.Attributes.strokeWidth (String.fromFloat sw)


strokeOpacityFromColor : Color -> Svg.Attribute msg
strokeOpacityFromColor color =
    strokeOpacity (Color.toRgba color).alpha


strokeOpacity : Float -> Svg.Attribute msg
strokeOpacity opacity =
    Svg.Attributes.strokeOpacity <|
        String.fromFloat opacity


getAlpha : Color -> Float
getAlpha color =
    Color.toRgba color |> .alpha


maybeFill : Maybe Color -> List (Svg.Attribute msg)
maybeFill maybeColor =
    case maybeColor of
        Just color ->
            [ fill color
            , fillOpacityFromColor color
            ]

        Nothing ->
            [ fillOpacity 0 ]


fill : Color -> Svg.Attribute msg
fill color =
    Svg.Attributes.fill (renderColor color)


fillOpacityFromColor : Color -> Svg.Attribute msg
fillOpacityFromColor color =
    fillOpacity (Color.toRgba color).alpha


fillOpacity : Float -> Svg.Attribute msg
fillOpacity opacity =
    Svg.Attributes.fillOpacity <|
        String.fromFloat opacity


renderColor : Color -> String
renderColor color =
    let
        { red, green, blue, alpha } =
            toRgba255 color
    in
    List.map String.fromInt [ red, green, blue ]
        |> String.join ","
        |> (\s -> "rgb(" ++ s ++ ")")


toRgba255 : Color -> { red : Int, green : Int, blue : Int, alpha : Int }
toRgba255 color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    { red = to255 red
    , green = to255 green
    , blue = to255 blue
    , alpha = to255 alpha
    }


to255 : Float -> Int
to255 value =
    round <| clamp 0 255 <| (value * 255)
