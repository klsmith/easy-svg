module EasySvg exposing (Camera, PortView, draw)

import Color exposing (Color)
import Drawable exposing (Drawable, DrawingData, Shape(..))
import EasySvg.Attributes exposing (..)
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes


type alias PortView =
    { width : Float
    , height : Float
    }


type alias Camera =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


draw : PortView -> Camera -> List Drawable -> Html msg
draw portView camera drawables =
    Svg.svg
        [ width portView.width
        , height portView.height
        , viewBox camera.x camera.y camera.width camera.height
        ]
        (List.map drawShape drawables)


drawShape : Drawable -> Svg msg
drawShape drawable =
    let
        data =
            Drawable.getDrawingData drawable
    in
    case data.shape of
        Circle radius ->
            circle data radius


circle : DrawingData -> Float -> Svg msg
circle data radius =
    Svg.circle
        ([ r radius ]
            ++ maybeOutline data.outline
            ++ maybeFill data.fill
            ++ position data.position
        )
        []
