module EasySvg exposing (Camera, PortView, draw)

import Color exposing (Color)
import Drawable exposing (Drawable, DrawingData, Shape(..))
import Html exposing (Html)
import Svg exposing (Attribute, Svg)
import TypedSvg as TS
import TypedSvg.Attributes as TSA
import TypedSvg.Types as TST


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
    TS.svg
        [ TSA.width (TST.num portView.width)
        , TSA.height (TST.num portView.height)
        , TSA.viewBox camera.x camera.y camera.width camera.height
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
            TS.circle
                [ TSA.r (TST.num radius)
                , TSA.cx (TST.num <| Tuple.first data.position)
                , TSA.cy (TST.num <| Tuple.second data.position)
                , TSA.stroke (getStrokePaint data)
                , TSA.strokeWidth (getStrokeWidth data)
                , TSA.fill (getFillPaint data)
                ]
                []

        Rectangle width height ->
            TS.rect
                [ TSA.width (TST.num width)
                , TSA.height (TST.num height)
                , TSA.x (getTopLeftX data width)
                , TSA.y (getTopLeftY data height)
                , TSA.stroke (getStrokePaint data)
                , TSA.strokeWidth (getStrokeWidth data)
                , TSA.fill (getFillPaint data)
                ]
                []


getTopLeftX : DrawingData -> Float -> TST.Length
getTopLeftX data width =
    TST.num <| (\x -> x - width / 2) <| Tuple.first data.position


getTopLeftY : DrawingData -> Float -> TST.Length
getTopLeftY data height =
    TST.num <| (\y -> y - height / 2) <| Tuple.second data.position


getStrokePaint : DrawingData -> TST.Paint
getStrokePaint data =
    Maybe.withDefault TST.PaintNone <| Maybe.map TST.Paint <| Maybe.map Tuple.first <| data.outline


getStrokeWidth : DrawingData -> TST.Length
getStrokeWidth data =
    TST.num <| Maybe.withDefault 0 <| Maybe.map Tuple.second <| data.outline


getFillPaint : DrawingData -> TST.Paint
getFillPaint data =
    Maybe.withDefault TST.PaintNone <| Maybe.map TST.Paint <| data.fill
