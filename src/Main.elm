module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Game exposing (..)
import Html exposing (Html)
import Icons
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg, g, svg)
import Svg.Attributes
import Time


type alias Model =
    { appPhase : AppPhase
    , viewportSize : ViewportSize
    }


type alias ViewportSize =
    { width : Int, height : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { appPhase = Welcome
      , viewportSize = { width = 600, height = 400 }
      }
    , Cmd.none
    )


type Msg
    = KeyDown UserControl
    | KeyUp UserControl
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.appPhase of
        Welcome ->
            case msg of
                KeyDown _ ->
                    ( { model | appPhase = Playing Game.init }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Playing gameState ->
            ( { model | appPhase = updateGameState msg gameState }, Cmd.none )

        GameOver ->
            case msg of
                KeyDown _ ->
                    ( { model | appPhase = Playing Game.init }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Congrats ->
            case msg of
                KeyDown _ ->
                    ( { model | appPhase = Playing Game.init }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


insertOnce : a -> List a -> List a
insertOnce elem list =
    if List.member elem list then
        list

    else
        elem :: list


updateGameState : Msg -> GameState -> AppPhase
updateGameState msg gameState =
    case msg of
        KeyDown control ->
            Playing { gameState | activeControls = insertOnce control gameState.activeControls }

        KeyUp control ->
            Playing { gameState | activeControls = List.filter ((/=) control) gameState.activeControls }

        Tick _ ->
            gameState
                |> evalUserControl
                |> evalStep
                |> evalHits
                |> evalResults


view : Model -> Html Msg
view model =
    let
        printScore score =
            score |> String.fromInt |> String.padLeft 5 '0'

        children =
            case model.appPhase of
                Welcome ->
                    [ column [ width fill, height fill ]
                        [ el [ centerX, centerY, Font.size 60, Font.center ] (text "Space\nInvaders")
                        , el [ centerX ] (text "Press any key to start")
                        ]
                    ]

                Playing gameState ->
                    [ row [ width fill ]
                        [ el [ centerX ] (text "Space invaders")
                        , el [ alignRight ] (text ("Score: " ++ printScore gameState.score))
                        ]
                    , row [] [ html <| viewGamePanel model.viewportSize gameState ]
                    ]

                GameOver ->
                    [ column [ width fill, height fill ]
                        [ el [ centerX, centerY, Font.size 30 ] (text "Game Over")
                        , el [ centerX ] (text "Press any key to restart")
                        ]
                    ]

                Congrats ->
                    [ column [ width fill, height fill ]
                        [ el [ centerX, centerY, Font.size 30 ] (text "Congratulations")
                        , el [ centerX ] (text "Press any key to restart")
                        ]
                    ]
    in
    layout []
        (column
            [ Background.color (rgb255 0 0 0)
            , Font.color (rgb255 200 200 200)
            , Font.family [ Font.typeface "Courier New", Font.monospace ]
            , width (px model.viewportSize.width)
            , height (px model.viewportSize.height)
            , spacing 10
            , padding 20
            ]
            children
        )


viewGamePanel : ViewportSize -> GameState -> Svg Msg
viewGamePanel viewportSize gameState =
    svg
        [ Svg.Attributes.viewBox "0 0 300 200"
        , Svg.Attributes.width (String.fromInt <| viewportSize.width - 40)
        , Svg.Attributes.height (String.fromInt <| viewportSize.height - 70)
        , Svg.Attributes.style "backgroundColor: black"
        ]
        (List.map Icons.viewUfo gameState.ufos.list
            ++ List.map Icons.viewLaser gameState.lasers.list
            ++ [ Icons.viewShip gameState.shipXPosition ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.appPhase of
        Welcome ->
            Sub.batch
                [ Browser.Events.onKeyDown (decodeKeyPress KeyDown)
                ]

        Playing _ ->
            Sub.batch
                [ Browser.Events.onKeyDown (decodeKeyPress KeyDown)
                , Browser.Events.onKeyUp (decodeKeyPress KeyUp)
                , Time.every 50 Tick
                ]

        GameOver ->
            Sub.batch
                [ Browser.Events.onKeyDown (decodeKeyPress KeyDown)
                ]

        Congrats ->
            Sub.batch
                [ Browser.Events.onKeyDown (decodeKeyPress KeyDown)
                ]


decodeKeyPress : (UserControl -> Msg) -> Decoder Msg
decodeKeyPress toMsg =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToUserControl
        |> Decode.map toMsg


keyToUserControl : String -> Decoder UserControl
keyToUserControl key =
    case key of
        "ArrowRight" ->
            Decode.succeed Right

        "ArrowLeft" ->
            Decode.succeed Left

        " " ->
            Decode.succeed Fire

        _ ->
            Decode.fail "not a direction key"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
