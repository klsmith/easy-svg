module EasySvg exposing
    ( Camera
    , Drawable
    , DrawingData
    , FontFamily(..)
    , PortView
    , Shape(..)
    , Transform(..)
    , circle
    , ellipse
    , fill
    , getDrawingData
    , group
    , hexagon
    , image
    , octagon
    , outline
    , pentagon
    , polygon
    , rectangle
    , render
    , rotate
    , scale
    , scaleX
    , scaleXY
    , scaleY
    , skewX
    , skewY
    , text
    , toSvg
    , translate
    , translateX
    , translateY
    , triangle
    )

import Color exposing (Color)
import Html exposing (Html)
import Svg exposing (Svg)
import TypedSvg as TS
import TypedSvg.Attributes as TSA
import TypedSvg.Types as TST



-- TYPES


type alias PortView =
    { width : Float
    , height : Float
    }


type alias Camera =
    { centerX : Float
    , centerY : Float
    , width : Float
    , height : Float
    }


type Drawable
    = Drawable DrawingData


type alias DrawingData =
    { shape : Shape
    , outline : Maybe ( Color, Float )
    , fill : Maybe Color
    , transforms : List Transform
    }


type Shape
    = Circle Float
    | Rectangle Float Float
    | Ellipse Float Float
    | Polygon (List Point)
    | Ngon Int Float
    | Image String Float Float
    | Text String FontFamily Float
    | Group (List Drawable)


type alias Point =
    ( Float, Float )


type FontFamily
    = Inherit
    | Family (List String)


type Transform
    = Translate Float Float
    | Rotate Float
    | Scale Float Float
    | SkewX Float
    | SkewY Float



-- CONSTRUCTORS


{-| Not public, this is used to hold the default data for everything except shape.
-}
drawable : Shape -> Drawable
drawable shape =
    Drawable
        { shape = shape
        , outline = Nothing
        , fill = Nothing
        , transforms = []
        }


circle : Float -> Drawable
circle =
    drawable << Circle


rectangle : Float -> Float -> Drawable
rectangle width height =
    drawable (Rectangle width height)


ellipse : Float -> Float -> Drawable
ellipse width height =
    drawable (Ellipse width height)


polygon : List Point -> Drawable
polygon =
    drawable << Polygon


triangle : Float -> Drawable
triangle =
    drawable << Ngon 3


pentagon : Float -> Drawable
pentagon =
    drawable << Ngon 5


hexagon : Float -> Drawable
hexagon =
    drawable << Ngon 6


octagon : Float -> Drawable
octagon =
    drawable << Ngon 8


image : Float -> Float -> String -> Drawable
image width height src =
    drawable (Image src width height)


text : FontFamily -> Float -> String -> Drawable
text fontFamily size string =
    drawable (Text string fontFamily size)


group : List Drawable -> Drawable
group =
    drawable << Group



-- GENERIC PIPELINE


{-| Exposed to allow for custom rendering modules.
-}
getDrawingData : Drawable -> DrawingData
getDrawingData (Drawable data) =
    data


applyTransform : Transform -> Drawable -> Drawable
applyTransform newTransform (Drawable data) =
    Drawable { data | transforms = newTransform :: data.transforms }


outline : Color -> Float -> Drawable -> Drawable
outline color width (Drawable data) =
    Drawable { data | outline = Just ( color, width ) }


fill : Color -> Drawable -> Drawable
fill color (Drawable data) =
    Drawable { data | fill = Just color }


rotate : Float -> Drawable -> Drawable
rotate newAngle =
    applyTransform (Rotate newAngle)



-- SCALE PIPELINE


scale : Float -> Drawable -> Drawable
scale s =
    scaleXY s s


scaleX : Float -> Drawable -> Drawable
scaleX x =
    scaleXY x 1


scaleY : Float -> Drawable -> Drawable
scaleY y =
    scaleXY 1 y


scaleXY : Float -> Float -> Drawable -> Drawable
scaleXY x y =
    applyTransform (Scale x y)



-- TRANSLATE PIPELINE


translateX : Float -> Drawable -> Drawable
translateX x =
    translate x 0


translateY : Float -> Drawable -> Drawable
translateY y =
    translate 0 y


translate : Float -> Float -> Drawable -> Drawable
translate x y =
    applyTransform (Translate x y)



-- SKEW PIPELINE


skewX : Float -> Drawable -> Drawable
skewX a =
    applyTransform (SkewX a)


skewY : Float -> Drawable -> Drawable
skewY a =
    applyTransform (SkewY a)



-- RENDERING


render : PortView -> Camera -> List Drawable -> Html msg
render portView camera drawables =
    TS.svg
        [ TSA.width (TST.num portView.width)
        , TSA.height (TST.num portView.height)
        , TSA.viewBox
            (centerToLeftX camera.centerX camera.width)
            (centerToTopY camera.centerY camera.height)
            camera.width
            camera.height
        ]
        (List.map toSvg drawables)


centerToLeftX : Float -> Float -> Float
centerToLeftX centerX width =
    centerX - width / 2


centerToTopY : Float -> Float -> Float
centerToTopY centerY height =
    centerY - height / 2


toSvg : Drawable -> Svg msg
toSvg (Drawable data) =
    case data.shape of
        Circle radius ->
            TS.circle
                (TSA.r (TST.num radius)
                    :: TSA.transform (getTransforms data)
                    :: commonAttributes data
                )
                []

        Rectangle width height ->
            TS.rect
                (TSA.width (TST.num width)
                    :: TSA.height (TST.num height)
                    :: TSA.transform (getRectTransforms data width height)
                    :: commonAttributes data
                )
                []

        Ellipse radiusX radiusY ->
            TS.ellipse
                (TSA.rx (TST.num radiusX)
                    :: TSA.ry (TST.num radiusY)
                    :: TSA.transform (getTransforms data)
                    :: commonAttributes data
                )
                []

        Polygon points ->
            TS.polygon
                (TSA.points points
                    :: TSA.transform (getTransforms data)
                    :: commonAttributes data
                )
                []

        Ngon n radius ->
            TS.polygon
                (TSA.points (toNgonPoints 0 n radius [])
                    :: TSA.transform (getTransforms data)
                    :: commonAttributes data
                )
                []

        Text string fontFamily size ->
            TS.text_
                (TSA.textAnchor TST.AnchorMiddle
                    :: TSA.dominantBaseline TST.DominantBaselineCentral
                    :: TSA.fontFamily (toFontFamilyList fontFamily)
                    :: TSA.fontSize (TST.num size)
                    :: TSA.transform (getTransforms data)
                    :: TSA.style disableTextSelect
                    :: commonAttributes data
                )
                [ Svg.text string ]

        Image src width height ->
            TS.image
                [ TSA.xlinkHref src
                , TSA.width (TST.num width)
                , TSA.height (TST.num height)
                , TSA.transform (getRectTransforms data width height)
                ]
                []

        Group drawables ->
            TS.g [ TSA.transform (getTransforms data) ]
                (List.map toSvg drawables)


{-| Stuff that every shape shares, except group and image
-}
commonAttributes : DrawingData -> List (Svg.Attribute msg)
commonAttributes data =
    [ TSA.strokeWidth (TST.num (getStrokeWidth data))
    , TSA.stroke (getStrokePaint data)
    , TSA.fill (getFillPaint data)
    ]


disableTextSelect : String
disableTextSelect =
    """-webkit-user-select: none;
-moz-user-select: none;
-ms-user-select: none;
user-select: none;"""


toFontFamilyList : FontFamily -> List String
toFontFamilyList fontFamily =
    case fontFamily of
        Inherit ->
            []

        Family list ->
            list


toNgonPoints : Int -> Int -> Float -> List ( Float, Float ) -> List ( Float, Float )
toNgonPoints i n radius list =
    if i == n then
        list

    else
        let
            a =
                turns (toFloat i / toFloat n - 0.25)

            x =
                radius * cos a

            y =
                radius * sin a
        in
        toNgonPoints (i + 1) n radius (( x, y ) :: list)


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


getRectTransforms : DrawingData -> Float -> Float -> List TST.Transform
getRectTransforms data width height =
    getTransforms data ++ [ TST.Translate (-width / 2) (-height / 2) ]


getTransforms : DrawingData -> List TST.Transform
getTransforms data =
    let
        ( translations, otherTransforms ) =
            List.partition
                (\t ->
                    case t of
                        Translate _ _ ->
                            True

                        _ ->
                            False
                )
                data.transforms
    in
    List.map mapTransformTypes translations
        ++ List.map mapTransformTypes otherTransforms


mapTransformTypes : Transform -> TST.Transform
mapTransformTypes t =
    case t of
        Translate x y ->
            TST.Translate x y

        Rotate a ->
            TST.Rotate -a 0 0

        Scale x y ->
            TST.Scale x y

        SkewX a ->
            TST.SkewX a

        SkewY a ->
            TST.SkewY a
