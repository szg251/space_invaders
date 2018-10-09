module Main exposing (main)

import Browser
import Browser.Events
import Game exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Icons
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg, g, svg)
import Svg.Attributes exposing (enableBackground, height, viewBox, width)
import Time


type alias Model =
    { appPhase : AppPhase
    , viewportSize : ViewportSize
    }


type alias ViewportSize =
    { width : Int, height : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { appPhase = Playing Game.init
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
        Playing gameState ->
            ( { model | appPhase = updateGameState msg gameState }, Cmd.none )

        GameOver ->
            ( model, Cmd.none )

        Congrats ->
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
                |> evalUfoStep (gameState.steps + 1)
                |> evalHits
                |> evalResults


view : Model -> Html Msg
view model =
    let
        children =
            case model.appPhase of
                Playing gameState ->
                    [ div
                        [ style "textAlign" "center"
                        , style "paddingBottom" "20px"
                        ]
                        [ text "Space Invaders" ]
                    , viewGamePanel model.viewportSize gameState
                    ]

                GameOver ->
                    [ div
                        [ style "textAlign" "center"
                        , style "fontSize" "8rem"
                        ]
                        [ text "Game Over" ]
                    ]

                Congrats ->
                    [ div
                        [ style "textAlign" "center"
                        , style "fontSize" "8rem"
                        ]
                        [ text "Congratulations" ]
                    ]
    in
    div
        [ style "backgroundColor" "black"
        , style "color" "white"
        , style "width" (toPx <| model.viewportSize.width)
        , style "height" (toPx <| model.viewportSize.height)
        , style "padding" "20px"
        , style "display" "inline-block"
        , style "fontFamily" "Courier New"
        ]
        children


toPx : Int -> String
toPx number =
    String.fromInt number |> (\x -> x ++ "px")


viewGamePanel : ViewportSize -> GameState -> Svg Msg
viewGamePanel viewportSize gameState =
    svg
        [ viewBox "0 0 300 200"
        , width (String.fromInt <| viewportSize.width - 40)
        , height (String.fromInt <| viewportSize.height - 40)
        , style "backgroundColor" "black"
        ]
        (List.map Icons.viewUfo gameState.ufos
            ++ List.map Icons.viewLaser gameState.lasers
            ++ [ Icons.viewShip gameState.shipXPosition ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.appPhase of
        Playing _ ->
            Sub.batch
                [ Browser.Events.onKeyDown (decodeKeyPress KeyDown)
                , Browser.Events.onKeyUp (decodeKeyPress KeyUp)
                , Time.every 100 Tick
                ]

        GameOver ->
            Sub.none

        Congrats ->
            Sub.none


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
