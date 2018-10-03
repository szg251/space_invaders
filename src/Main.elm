module Main exposing (AppPhase(..), GameState, Laser, Model, Msg(..), ShipAction(..), Ufo, ViewportSize, calcHits, decodeKeyPress, evalHits, evalStep, init, initUfos, keyToShipAction, main, stepLasers, stepUfo, subscriptions, subtractList, toPx, update, updateGameState, view, viewGamePanel)

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


type alias GameState =
    { shipXPosition : Int
    , ufos : List Ufo
    , steps : Int
    , lasers : List Laser
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


type ShipAction
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
    = KeyPress ShipAction
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.appPhase of
        Playing gameState ->
            ( { model | appPhase = updateGameState msg gameState }, Cmd.none )

        GameOver ->
            ( model, Cmd.none )


updateGameState : Msg -> GameState -> AppPhase
updateGameState msg gameState =
    case msg of
        KeyPress shipAction ->
            case shipAction of
                Right ->
                    Playing { gameState | shipXPosition = min 290 (gameState.shipXPosition + 5) }

                Left ->
                    Playing { gameState | shipXPosition = max 0 (gameState.shipXPosition - 5) }

                Fire ->
                    Playing { gameState | lasers = Laser (gameState.shipXPosition + 5) 180 :: gameState.lasers }

        Tick _ ->
            let
                nextState =
                    evalStep (gameState.steps + 1) gameState
                        |> Maybe.map evalHits
            in
            case nextState of
                Just afterStep ->
                    Playing afterStep

                Nothing ->
                    GameOver


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


evalStep : Int -> GameState -> Maybe GameState
evalStep nextStep gameState =
    let
        ufoTickSkip =
            5
    in
    if nextStep < (110 * ufoTickSkip) then
        Just
            { gameState
                | steps = nextStep
                , ufos = List.map (stepUfo ufoTickSkip nextStep) gameState.ufos
                , lasers = stepLasers gameState.lasers
            }

    else
        Nothing


evalHits : GameState -> GameState
evalHits ({ lasers, ufos } as gameState) =
    let
        ( laserHits, ufoHits ) =
            calcHits lasers ufos
                |> List.unzip
    in
    { gameState | lasers = subtractList lasers laserHits, ufos = subtractList ufos ufoHits }


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
                [ Browser.Events.onKeyDown decodeKeyPress
                , Time.every 100 Tick
                ]

        GameOver ->
            Sub.none


decodeKeyPress : Decoder Msg
decodeKeyPress =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToShipAction
        |> Decode.map KeyPress


keyToShipAction : String -> Decoder ShipAction
keyToShipAction key =
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
