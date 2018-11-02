module Icons exposing (viewLaser, viewShip, viewUfo, volumeOff, volumeUp)

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


volumeUp : Svg msg
volumeUp =
    svg [ width "24", height "24", viewBox "0 0 24 24" ]
        [ path
            [ d "M3 9v6h4l5 5V4L7 9H3zm13.5 3c0-1.77-1.02-3.29-2.5-4.03v8.05c1.48-.73 2.5-2.25 2.5-4.02zM14 3.23v2.06c2.89.86 5 3.54 5 6.71s-2.11 5.85-5 6.71v2.06c4.01-.91 7-4.49 7-8.77s-2.99-7.86-7-8.77z"
            , fill "currentColor"
            ]
            []
        , path
            [ d "M0 0h24v24H0z", fill "none" ]
            []
        ]


volumeOff : Svg msg
volumeOff =
    svg [ width "24", height "24", viewBox "0 0 24 24" ]
        [ path [ d "M16.5 12c0-1.77-1.02-3.29-2.5-4.03v2.21l2.45 2.45c.03-.2.05-.41.05-.63zm2.5 0c0 .94-.2 1.82-.54 2.64l1.51 1.51C20.63 14.91 21 13.5 21 12c0-4.28-2.99-7.86-7-8.77v2.06c2.89.86 5 3.54 5 6.71zM4.27 3L3 4.27 7.73 9H3v6h4l5 5v-6.73l4.25 4.25c-.67.52-1.42.93-2.25 1.18v2.06c1.38-.31 2.63-.95 3.69-1.81L19.73 21 21 19.73l-9-9L4.27 3zM12 4L9.91 6.09 12 8.18V4z", fill "currentColor" ] []
        , path [ d "M0 0h24v24H0z", fill "none" ] []
        ]
