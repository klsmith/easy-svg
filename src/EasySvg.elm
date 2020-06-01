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


drawShape : Drawable -> Svg a
drawShape drawable =
    let
        data =
            Drawable.getDrawingData drawable
    in
    Shape.mapAny data.shape
        { circle = circle data
        }


circle : DrawingData -> Float -> Svg a
circle data radius =
    Svg.circle
        (r radius
            :: cx (Tuple.first data.position)
            :: cy (Tuple.second data.position)
            :: fill data.fill
            :: []
        )
        []


maybeAttr : (a -> Svg.Attribute msg) -> Maybe a -> List (Svg.Attribute msg)
maybeAttr mapper mv =
    Maybe.map mapper mv
        |> Maybe.map List.singleton
        |> Maybe.withDefault []


fill : Maybe Color -> Svg.Attribute msg
fill maybeColor =
    Svg.Attributes.fill (renderColor maybeColor)


renderColor : Maybe Color -> String
renderColor maybeColor =
    let
        { red, green, blue, alpha } =
            toRgba255 (Maybe.withDefault noColor maybeColor)
    in
    List.map String.fromInt [ red, green, blue, alpha ]
        |> String.join ","
        |> (\s -> "rgba(" ++ s ++ ")")


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
    clamp 0 255 <| truncate <| value * 255


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


cx : Float -> Svg.Attribute msg
cx =
    Svg.Attributes.cx << String.fromFloat


cy : Float -> Svg.Attribute msg
cy =
    Svg.Attributes.cy << String.fromFloat
