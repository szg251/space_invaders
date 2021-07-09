module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Game exposing (..)
import Html exposing (Html)
import Icons
import Json.Decode as Decode exposing (Decoder)
import Sound
import Svg exposing (Svg, g, svg)
import Svg.Attributes
import Task
import Time


type alias Model =
    { appPhase : AppPhase
    , viewportSize : ViewportSize
    , isMuted : Bool
    }


type alias ViewportSize =
    { width : Int, height : Int }


toViewportSize : Browser.Dom.Viewport -> ViewportSize
toViewportSize { viewport } =
    { width = floor viewport.width
    , height = floor viewport.height
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { appPhase = Welcome
      , viewportSize = { width = 600, height = 400 }
      , isMuted = True
      }
    , Task.perform (ViewportResize << toViewportSize) Browser.Dom.getViewport
    )


type Msg
    = GameMsg Game.Msg
    | KeyPress
    | ToggleMute
    | ViewportResize ViewportSize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress ->
            ( { model | appPhase = Playing Game.init }, Cmd.none )

        ToggleMute ->
            ( { model | isMuted = not model.isMuted }, Cmd.none )

        GameMsg gameMsg ->
            case model.appPhase of
                Playing gameState ->
                    let
                        ( updatedGameState, sounds ) =
                            Game.update gameMsg gameState
                    in
                    ( { model | appPhase = updatedGameState }
                    , if model.isMuted then
                        Cmd.none

                      else
                        Sound.play sounds
                    )

                _ ->
                    ( model, Cmd.none )

        ViewportResize size ->
            ( { model | viewportSize = size }, Cmd.none )


fontUnscii8 : Element.Attribute Msg
fontUnscii8 =
    Font.family
        [ Font.typeface "unscii-8"
        , Font.monospace
        ]


fontUnscii16 : Element.Attribute Msg
fontUnscii16 =
    Font.family
        [ Font.typeface "unscii-16"
        , Font.monospace
        ]


view : Model -> Html Msg
view model =
    let
        volumeIcon =
            if model.isMuted then
                Icons.volumeOff

            else
                Icons.volumeUp

        printScore score =
            score |> String.fromInt |> String.padLeft 5 '0'

        children =
            case model.appPhase of
                Welcome ->
                    [ el [ centerX, centerY, fontUnscii16, Font.size 60, Font.center ] (text "Space\nInvaders")
                    , el [ centerX ] (text "Press any key to start")
                    ]

                Playing gameState ->
                    [ row [ width fill ]
                        [ el [ centerX ] (text "Space invaders")
                        , el [ alignRight ] (text ("Score: " ++ printScore gameState.score))
                        ]
                    , row [] [ html <| viewGamePanel model.viewportSize gameState ]
                    ]

                GameOver ->
                    [ el [ centerX, centerY, fontUnscii16, Font.size 30 ] (text "Game Over")
                    , el [ centerX ] (text "Press any key to restart")
                    ]

                Congrats ->
                    [ el [ centerX, centerY, fontUnscii16, Font.size 30 ] (text "Congratulations")
                    , el [ centerX ] (text "Press any key to restart")
                    ]
    in
    layout []
        (column
            [ Background.color (rgb255 0 0 0)
            , Font.color (rgb255 200 200 200)
            , fontUnscii8
            , width (px model.viewportSize.width)
            , height (px model.viewportSize.height)
            , spacing 10
            , padding 20
            ]
            [ column [ width fill, height fill, spacing 10 ] children
            , el [ alignRight, onClick ToggleMute ] (html volumeIcon)
            ]
        )


viewPage : String -> List (Element Msg)
viewPage title =
    [ column [ width fill, height fill ]
        [ el [ centerX, centerY, Font.size 30 ] (text "Congratulations")
        , el [ centerX ] (text "Press any key to restart")
        , el [ alignRight, onClick ToggleMute ] (html Icons.volumeUp)
        ]
    ]


viewGamePanel : ViewportSize -> GameState -> Svg Msg
viewGamePanel viewportSize gameState =
    svg
        [ Svg.Attributes.viewBox "0 0 300 200"
        , Svg.Attributes.width (String.fromInt <| viewportSize.width - 40)
        , Svg.Attributes.height (String.fromInt <| viewportSize.height - 110)
        , Svg.Attributes.style "backgroundColor: black"
        ]
        (List.map Icons.viewUfo gameState.ufos.list
            ++ List.map Icons.viewLaser gameState.lasers.list
            ++ [ Icons.viewShip gameState.shipXPosition ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        phaseDependentSubs =
            case model.appPhase of
                Playing _ ->
                    Game.subscriptions |> Sub.map GameMsg

                _ ->
                    Browser.Events.onKeyDown (Decode.succeed KeyPress)
    in
    Sub.batch
        [ phaseDependentSubs
        , Browser.Events.onResize (\width height -> ViewportResize { width = width, height = height })
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
