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
                , TSA.cx (TST.num data.x)
                , TSA.cy (TST.num data.y)
                , TSA.strokeWidth (TST.num <| getStrokeWidth data)
                , TSA.stroke (getStrokePaint data)
                , TSA.fill (getFillPaint data)
                ]
                []

        Rectangle width height ->
            TS.rect
                [ TSA.width (TST.num width)
                , TSA.height (TST.num height)
                , TSA.x (TST.num <| getTopLeftX data width)
                , TSA.y (TST.num <| getTopLeftY data height)
                , TSA.strokeWidth (TST.num <| getStrokeWidth data)
                , TSA.stroke (getStrokePaint data)
                , TSA.fill (getFillPaint data)
                ]
                []


getTopLeftX : DrawingData -> Float -> Float
getTopLeftX data width =
    data.x - width / 2


getTopLeftY : DrawingData -> Float -> Float
getTopLeftY data height =
    data.y - height / 2


getStrokeWidth : DrawingData -> Float
getStrokeWidth data =
    data.outline
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 0


getStrokePaint : DrawingData -> TST.Paint
getStrokePaint data =
    data.outline
        |> Maybe.map Tuple.first
        |> Maybe.map TST.Paint
        |> Maybe.withDefault TST.PaintNone


getFillPaint : DrawingData -> TST.Paint
getFillPaint data =
    data.fill
        |> Maybe.map TST.Paint
        |> Maybe.withDefault TST.PaintNone
