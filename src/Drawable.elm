module Drawable exposing
    ( Drawable
    , DrawingData
    , Shape(..)
    , Transform(..)
    , circle
    , ellipse
    , fill
    , getDrawingData
    , group
    , outline
    , pentagon
    , polygon
    , rectangle
    , rotate
    , scale
    , scaleX
    , scaleXY
    , scaleY
    , skewX
    , skewY
    , translate
    , translateX
    , translateY
    , triangle
    )

import Color exposing (Color)


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
    | Ngon Int Float
    | Polygon (List Point)
    | Group (List Drawable)


type alias Point =
    ( Float, Float )


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


octogon : Float -> Drawable
octogon =
    drawable << Ngon 8


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



-- POSITION PIPELINE


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
