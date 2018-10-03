module Icons exposing (viewLaser, viewShip, viewUfo)

import Svg exposing (..)
import Svg.Attributes exposing (cx, cy, d, enableBackground, fill, height, r, stroke, transform, viewBox, width, x, x1, x2, y, y1, y2)


viewUfo : { r | x : Int, y : Int } -> Svg msg
viewUfo ufo =
    svg
        [ viewBox "0 0 1000 1000"
        , enableBackground "new 0 0 1000 1000"
        , width "10"
        , height "10"
        , fill "red"
        , x (String.fromInt ufo.x)
        , y (String.fromInt ufo.y)
        ]
        [ g
            [ transform "translate(0,511) scale(0.1,-0.1)"
            ]
            [ path [ d "M2055.4,4285.3v-483.1h483.1h483.1v483.1v483.1h-483.1h-483.1V4285.3z" ] []
            , path [ d "M6978.4,4285.3v-483.1h483.1h483.1v483.1v483.1h-483.1h-483.1V4285.3z" ] []
            , path [ d "M3044.6,3284.6V2790H2550h-494.6v-483.1v-483.1h-483.1h-483.1v-494.6V834.6H594.6H100v-2196.9v-2196.9h483.1h483.1v977.7v977.7h253.1h253.1v-977.7v-977.7h483.1h483.1v483.1v483.1H5000h2461.5v-483.1v-483.1h483.1h483.1v977.7v977.7h253.1h253v-977.7v-977.7h483.1H9900v2196.9V834.6h-494.6h-494.6v494.6v494.6h-483.1h-483.1v483.1V2790H7450h-494.6v494.6v494.6h-483.1h-483.1v-494.6V2790H5000h-989.2v494.6v494.6h-483.1h-483.1V3284.6z M4033.8-143.1v-494.6h-506.1h-506.1v494.6v494.6h506.1h506.1V-143.1z M6978.4-143.1v-494.6h-506.1h-506.1v494.6v494.6h506.1h506.1V-143.1z" ] []
            , path [ d "M2561.5-4065.4v-483.1h966.2h966.2v483.1v483.1h-966.2h-966.2V-4065.4z" ] []
            , path [ d "M5506.1-4065.4v-483.1h966.2h966.2v483.1v483.1h-966.2h-966.2V-4065.4z" ] []
            ]
        ]


viewShip : Int -> Svg msg
viewShip position =
    svg
        [ viewBox "0 0 200 200"
        , fill "white"
        , x (String.fromInt position)
        , y "190"
        , width "10"
        , height "10"
        ]
        [ g []
            [ path [ d "M0 127h176v65H0z" ] []
            , path [ d "M31 96h115v66H31z" ] []
            , path [ d "M63 64h48v65h-48z" ] []
            ]
        ]


viewLaser : { r | x : Int, y : Int } -> Svg msg
viewLaser laser =
    line
        [ stroke "yellow"
        , x1 (String.fromInt laser.x)
        , y1 (String.fromInt laser.y)
        , x2 (String.fromInt laser.x)
        , y2 (String.fromInt (laser.y + 10))
        ]
        []



-- svg
--     [ viewBox "0 0 200 200"
--     , fill "yellow"
--     , x (String.fromInt laser.x)
--     , y (String.fromInt laser.y)
--     , width "10"
--     , height "10"
--     ]
--     [ g []
--         [ path [ d "M0 127h176v65H0z" ] []
--         , path [ d "M31 96h115v66H31z" ] []
--         , path [ d "M63 64h48v65h-48z" ] []
--         ]
--     ]
