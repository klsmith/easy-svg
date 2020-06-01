module Drawable exposing
    ( Drawable
    , DrawingData
    , drawable
    , fill
    , getDrawingData
    , outline
    , position
    )

import Color exposing (Color)
import Shape exposing (Shape)


type Drawable
    = Drawable DrawingData


type alias DrawingData =
    { shape : Shape
    , image : Maybe String
    , outline : Maybe ( Color, Float )
    , fill : Maybe Color
    , scale : ( Float, Float )
    , skew : ( Float, Float )
    , position : ( Float, Float )
    , rotate : Float
    }



-- CONSTRUCTORS


drawable : Shape -> Drawable
drawable shape =
    Drawable
        { shape = shape
        , image = Nothing
        , outline = Nothing
        , fill = Nothing
        , scale = ( 1, 1 )
        , skew = ( 0, 0 )
        , position = ( 0, 0 )
        , rotate = 0
        }



-- GENERIC PIPELINE


{-| Doesn't Render
-}
outline : Color -> Float -> Drawable -> Drawable
outline color width (Drawable data) =
    Drawable { data | outline = Just ( color, width ) }


fill : Color -> Drawable -> Drawable
fill color (Drawable data) =
    Drawable { data | fill = Just color }


{-| Doesn't Render
-}
image : String -> Drawable -> Drawable
image imgSrc (Drawable data) =
    Drawable { data | image = Just imgSrc }


{-| Doesn't Render
-}
rotate : Float -> Drawable -> Drawable
rotate r (Drawable data) =
    Drawable { data | rotate = r }



-- SCALE PIPELINE


{-| Doesn't Render
-}
scale : Float -> Drawable -> Drawable
scale s d =
    d |> scaleXY s s


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



-- SKEW PIPELINE


{-| Doesn't Render
-}
skew : Float -> Drawable -> Drawable
skew s d =
    d |> skewXY s s


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



-- GENERIC GETTERS


getDrawingData : Drawable -> DrawingData
getDrawingData (Drawable data) =
    data


getShape : Drawable -> Shape
getShape (Drawable data) =
    data.shape


getOutline : Drawable -> Maybe ( Color, Float )
getOutline (Drawable data) =
    data.outline


getFill : Drawable -> Maybe Color
getFill (Drawable data) =
    data.fill


getImageSrc : Drawable -> Maybe String
getImageSrc (Drawable data) =
    data.image


getRotation : Drawable -> Float
getRotation (Drawable data) =
    data.rotate



-- SCALE GETTERS


getScaleXY : Drawable -> ( Float, Float )
getScaleXY (Drawable data) =
    data.scale


getScaleX : Drawable -> Float
getScaleX d =
    Tuple.first (getScaleXY d)


getScaleY : Drawable -> Float
getScaleY d =
    Tuple.second (getScaleXY d)



-- SKEW GETTERS


getSkewXY : Drawable -> ( Float, Float )
getSkewXY (Drawable data) =
    data.skew


getSkewX : Drawable -> Float
getSkewX d =
    Tuple.first (getSkewXY d)


getSkewY : Drawable -> Float
getSkewY d =
    Tuple.second (getSkewXY d)



-- POSITION GETTERS


getPosition : Drawable -> ( Float, Float )
getPosition (Drawable data) =
    data.position


getPositionX : Drawable -> Float
getPositionX d =
    Tuple.first (getPosition d)


getPositionY : Drawable -> Float
getPositionY d =
    Tuple.second (getPosition d)
