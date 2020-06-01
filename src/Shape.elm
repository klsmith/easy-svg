module Shape exposing (Shape(..), circle, mapAny)


type Shape
    = Circle Float



-- MAPPING FUNCTIONS


mapAny :
    Shape
    -> { circle : Float -> a }
    -> a
mapAny shape mappings =
    case shape of
        Circle radius ->
            mappings.circle radius



-- CIRCLE


circle : Float -> Shape
circle radius =
    Circle radius
