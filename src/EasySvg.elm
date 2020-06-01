module EasySvg exposing (View, draw)

import Color exposing (Color)
import Drawable exposing (Drawable, DrawingData)
import Html exposing (Html)
import Shape exposing (Shape)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias View =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


draw : View -> List Drawable -> Html msg
draw view drawables =
    Svg.svg
        [ width view.width
        , height view.height
        , viewBox view.x view.y view.width view.height
        ]
        (List.map drawShape drawables)


drawShape : Drawable -> Svg msg
drawShape drawable =
    let
        data =
            Drawable.getDrawingData drawable
    in
    Shape.mapAny data.shape
        { circle = circle data
        }


circle : DrawingData -> Float -> Svg msg
circle data radius =
    Svg.circle
        (r radius
            :: fill data.fill
            ++ position data.position
            ++ outline data.outline
        )
        []


fill : Maybe Color -> List (Svg.Attribute msg)
fill maybeColor =
    case maybeColor of
        Just color ->
            [ Svg.Attributes.fill (renderColor color)
            , Svg.Attributes.fillOpacity <| String.fromFloat (Color.toRgba color).alpha
            ]

        Nothing ->
            [ Svg.Attributes.fillOpacity "0" ]


outline : Maybe ( Color, Float ) -> List (Svg.Attribute msg)
outline maybeOutline =
    case maybeOutline of
        Just ( color, strokeWidth ) ->
            [ Svg.Attributes.stroke (renderColor color)
            , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
            , Svg.Attributes.strokeOpacity <| String.fromFloat (Color.toRgba color).alpha
            ]

        Nothing ->
            [ Svg.Attributes.strokeOpacity "0" ]


renderColor : Color -> String
renderColor color =
    let
        { red, green, blue, alpha } =
            toRgba255 color
    in
    List.map String.fromInt [ red, green, blue ]
        |> String.join ","
        |> (\s -> "rgb(" ++ s ++ ")")


noColor : Color
noColor =
    Color.rgba 0 0 0 0


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
