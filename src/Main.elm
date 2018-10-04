module Main exposing (main)

import Browser
import Browser.Events
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


type AppPhase
    = Playing GameState
    | GameOver
    | Congrats


type alias GameState =
    { shipXPosition : Int
    , ufos : List Ufo
    , steps : Int
    , lasers : List Laser
    , activeControls : List UserControl
    }


type alias ViewportSize =
    { width : Int, height : Int }


type alias Laser =
    { x : Int
    , y : Int
    }


type alias Ufo =
    { x : Int
    , y : Int
    }


type UserControl
    = Left
    | Right
    | Fire


init : () -> ( Model, Cmd Msg )
init _ =
    ( { appPhase =
            Playing
                { shipXPosition = 135
                , ufos = initUfos
                , steps = 0
                , lasers = []
                , activeControls = []
                }
      , viewportSize = { width = 600, height = 400 }
      }
    , Cmd.none
    )


initUfos : List Ufo
initUfos =
    let
        xs =
            List.range 0 10
                |> List.map ((*) 20)

        ys =
            List.range 0 4
                |> List.map ((*) 20)
    in
    List.concatMap (\x -> List.map (\y -> Ufo x y) ys) xs


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


evalUserControl : GameState -> GameState
evalUserControl ({ activeControls, shipXPosition, lasers } as gameState) =
    let
        evalLeft state =
            if List.member Left activeControls then
                { state | shipXPosition = max 0 (state.shipXPosition - 5) }

            else
                state

        evalRight state =
            if List.member Right activeControls then
                { state | shipXPosition = min 290 (state.shipXPosition + 5) }

            else
                state

        evalFire state =
            if List.member Fire activeControls then
                { state | lasers = Laser (state.shipXPosition + 5) 180 :: state.lasers }

            else
                state
    in
    gameState
        |> evalLeft
        |> evalRight
        |> evalFire


stepLasers : List Laser -> List Laser
stepLasers =
    List.map (\laser -> { laser | y = laser.y - 10 })
        >> List.filter (\{ y } -> y >= 0)


stepUfo : Int -> Int -> Ufo -> Ufo
stepUfo tickSkip steps ufo =
    let
        stepsInRow =
            10

        currentStep =
            remainderBy (2 * stepsInRow) (steps // tickSkip)
    in
    if remainderBy tickSkip steps == 0 then
        if currentStep == 0 || currentStep == stepsInRow then
            { ufo | y = ufo.y + 10 }

        else if stepsInRow < currentStep then
            { ufo | x = ufo.x - 10 }

        else
            { ufo | x = ufo.x + 10 }

    else
        ufo


evalUfoStep : Int -> GameState -> GameState
evalUfoStep nextStep gameState =
    let
        ufoTickSkip =
            5
    in
    { gameState
        | steps = nextStep
        , ufos = List.map (stepUfo ufoTickSkip nextStep) gameState.ufos
        , lasers = stepLasers gameState.lasers
    }


evalHits : GameState -> GameState
evalHits ({ lasers, ufos } as gameState) =
    let
        ( laserHits, ufoHits ) =
            calcHits lasers ufos
                |> List.unzip
    in
    { gameState | lasers = subtractList lasers laserHits, ufos = subtractList ufos ufoHits }


evalResults : GameState -> AppPhase
evalResults gameState =
    if List.length gameState.ufos == 0 then
        Congrats

    else if List.foldl (\ufo lowest -> max ufo.y lowest) 0 gameState.ufos < 180 then
        Playing gameState

    else
        GameOver


calcHits : List Laser -> List Ufo -> List ( Laser, Ufo )
calcHits lasers ufos =
    let
        isOverlap laser ufo =
            (ufo.x <= laser.x && laser.x <= ufo.x + 10)
                && (ufo.y <= laser.y && laser.y + 10 <= ufo.y + 10)
    in
    doubleFoldl
        (\laser ufo hits ->
            if isOverlap laser ufo then
                ( laser, ufo ) :: hits

            else
                hits
        )
        ufos
        lasers


doubleFoldl : (a -> b -> List c -> List c) -> List a -> List b -> List c
doubleFoldl fn listA listB =
    List.foldl (\b accB -> List.foldl (\a accA -> fn a b accA) [] listA ++ accB) [] listB


subtractList : List a -> List a -> List a
subtractList baseList subtract =
    List.foldl
        (\base acc ->
            if List.member base subtract then
                acc

            else
                base :: acc
        )
        []
        baseList


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
