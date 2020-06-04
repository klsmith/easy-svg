module Drawable exposing
    ( Drawable
    , DrawingData
    , Shape(..)
    , circle
    , fill
    , getDrawingData
    , outline
    , position
    , rectangle
    )

import Color exposing (Color)


type Drawable
    = Drawable DrawingData


type alias DrawingData =
    { shape : Shape
    , outline : Maybe ( Color, Float )
    , fill : Maybe Color
    , scale : ( Float, Float )
    , skew : ( Float, Float )
    , position : ( Float, Float )
    , rotate : Float
    }


type Shape
    = Circle Float
    | Rectangle Float Float



-- CONSTRUCTORS


{-| Not public, this is used to hold the default data for everything except shape.
-}
drawable : Shape -> Drawable
drawable shape =
    Drawable
        { shape = shape
        , outline = Nothing
        , fill = Nothing
        , scale = ( 1, 1 )
        , skew = ( 0, 0 )
        , position = ( 0, 0 )
        , rotate = 0
        }


circle : Float -> Drawable
circle =
    drawable << Circle


rectangle : Float -> Float -> Drawable
rectangle width height =
    drawable (Rectangle width height)



-- GENERIC PIPELINE


{-| Exposed to allow for custom rendering modules.
-}
getDrawingData : Drawable -> DrawingData
getDrawingData (Drawable data) =
    data


outline : Color -> Float -> Drawable -> Drawable
outline color width (Drawable data) =
    Drawable { data | outline = Just ( color, width ) }


fill : Color -> Drawable -> Drawable
fill color (Drawable data) =
    Drawable { data | fill = Just color }


{-| Doesn't Render
-}
rotate : Float -> Drawable -> Drawable
rotate r (Drawable data) =
    Drawable { data | rotate = r }



-- SCALE PIPELINE


{-| Doesn't Render
-}
scale : Float -> Drawable -> Drawable
scale s =
    scaleXY s s


{-| Doesn't Render
-}
scaleXY : Float -> Float -> Drawable -> Drawable
scaleXY x y (Drawable data) =
    Drawable { data | scale = ( x, y ) }


{-| Doesn't Render
-}
scaleX : Float -> Drawable -> Drawable
scaleX x d =
    d |> scaleXY x (getScaleY d)


{-| Doesn't Render
-}
scaleY : Float -> Drawable -> Drawable
scaleY y d =
    d |> scaleXY (getScaleX d) y


getScaleXY : Drawable -> ( Float, Float )
getScaleXY =
    getDrawingData >> .scale


getScaleX : Drawable -> Float
getScaleX =
    getScaleXY >> Tuple.first


getScaleY : Drawable -> Float
getScaleY =
    getScaleXY >> Tuple.second



-- SKEW PIPELINE


{-| Doesn't Render
-}
skew : Float -> Drawable -> Drawable
skew s =
    skewXY s s


{-| Doesn't Render
-}
skewXY : Float -> Float -> Drawable -> Drawable
skewXY x y (Drawable data) =
    Drawable { data | skew = ( x, y ) }


{-| Doesn't Render
-}
skewX : Float -> Drawable -> Drawable
skewX x d =
    d |> skewXY x (getSkewY d)


{-| Doesn't Render
-}
skewY : Float -> Drawable -> Drawable
skewY y d =
    d |> skewXY (getSkewX d) y


getSkewXY : Drawable -> ( Float, Float )
getSkewXY =
    getDrawingData >> .skew


getSkewX : Drawable -> Float
getSkewX =
    getSkewXY >> Tuple.first


getSkewY : Drawable -> Float
getSkewY =
    getSkewXY >> Tuple.second



-- POSITION PIPELINE


position : Float -> Float -> Drawable -> Drawable
position x y (Drawable data) =
    Drawable { data | position = ( x, y ) }


positionX : Float -> Drawable -> Drawable
positionX x d =
    d |> position x (getPositionY d)


positionY : Float -> Drawable -> Drawable
positionY y d =
    d |> position (getPositionX d) y


getPosition : Drawable -> ( Float, Float )
getPosition =
    getDrawingData >> .position


getPositionX : Drawable -> Float
getPositionX =
    getPosition >> Tuple.first


getPositionY : Drawable -> Float
getPositionY =
    getPosition >> Tuple.second
