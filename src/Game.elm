module Game exposing
    ( AppPhase(..)
    , GameState
    , Laser
    , Msg(..)
    , Ufo
    , UserControl(..)
    , calcHits
    , evalHits
    , evalResults
    , evalStep
    , evalUserControl
    , init
    , subscriptions
    , update
    )

import Browser.Events
import GenericSet exposing (GenericSet)
import Json.Decode as Decode exposing (Decoder)
import Sound exposing (Sound)
import Time



-- Game constants


laserFrequency : Int
laserFrequency =
    8


laserSpeed : Int
laserSpeed =
    2


type Msg
    = KeyDown UserControl
    | KeyUp UserControl
    | Tick Time.Posix


type AppPhase
    = Welcome
    | Playing GameState
    | GameOver
    | Congrats


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


type alias Ufos =
    { list : List Ufo
    , waitStep : Int
    , steps : Int
    , stepFrequency : Int
    }


type alias Lasers =
    { list : List Laser
    , waitStep : Int
    }


type alias GameState =
    { shipXPosition : Int
    , ufos : Ufos
    , lasers : Lasers
    , activeControls : GenericSet UserControl
    , startPosition : Int
    , score : Int
    }


init : GameState
init =
    { shipXPosition = 135
    , ufos = initUfos 0
    , lasers = { list = [], waitStep = 0 }
    , activeControls = GenericSet.empty
    , startPosition = 0
    , score = 0
    }


update : Msg -> GameState -> ( AppPhase, List Sound )
update msg gameState =
    case msg of
        KeyDown control ->
            ( Playing { gameState | activeControls = GenericSet.insert control gameState.activeControls }, [] )

        KeyUp control ->
            ( Playing { gameState | activeControls = GenericSet.remove control gameState.activeControls }, [] )

        Tick _ ->
            ( gameState, [] )
                |> evalUserControl
                |> evalStep
                |> evalHits
                |> evalResults


initUfos : Int -> Ufos
initUfos startPosition =
    let
        xs =
            List.range 0 10
                |> List.map ((*) 20)

        ys =
            List.range 0 4
                |> List.map ((*) 20 >> (+) startPosition)
    in
    { list = List.concatMap (\x -> List.map (\y -> Ufo x y) ys) xs
    , waitStep = 0
    , steps = 1
    , stepFrequency = 20
    }


evalUserControl : ( GameState, List Sound ) -> ( GameState, List Sound )
evalUserControl ( { activeControls, shipXPosition, lasers } as oldState, oldSounds ) =
    let
        evalLeft ( state, sounds ) =
            if GenericSet.member Left activeControls then
                ( { state | shipXPosition = max 0 (state.shipXPosition - 5) }, sounds )

            else
                ( state, sounds )

        evalRight ( state, sounds ) =
            if GenericSet.member Right activeControls then
                ( { state | shipXPosition = min 290 (state.shipXPosition + 5) }, sounds )

            else
                ( state, sounds )

        evalFire ( state, sounds ) =
            if GenericSet.member Fire activeControls && state.lasers.waitStep == 0 then
                ( { state | lasers = shootLaser (state.shipXPosition + 5) state.lasers }, Sound.Fire :: sounds )

            else
                ( state, sounds )
    in
    ( oldState, oldSounds )
        |> evalLeft
        |> evalRight
        |> evalFire


shootLaser : Int -> Lasers -> Lasers
shootLaser x lasers =
    { lasers | list = Laser x 180 :: lasers.list, waitStep = laserFrequency }


stepLaser : Laser -> Laser
stepLaser laser =
    { laser | y = laser.y - 10 }


stepLasers : Lasers -> Lasers
stepLasers lasers =
    { lasers
        | list = List.map stepLaser lasers.list |> List.filter (\{ y } -> y >= 0)
        , waitStep = max 0 (lasers.waitStep - 1)
    }


stepUfo : Int -> Ufo -> Ufo
stepUfo steps ufo =
    let
        stepsInRow =
            10

        currentStep =
            remainderBy (2 * stepsInRow) steps
    in
    if currentStep == 0 || currentStep == stepsInRow then
        { ufo | y = ufo.y + 10 }

    else if stepsInRow < currentStep then
        { ufo | x = ufo.x - 10 }

    else
        { ufo | x = ufo.x + 10 }


stepUfos : Ufos -> Ufos
stepUfos ufos =
    if ufos.waitStep == 0 then
        { ufos
            | waitStep = ufos.stepFrequency
            , list = List.map (stepUfo ufos.steps) ufos.list
            , steps = ufos.steps + 1
            , stepFrequency = List.length ufos.list // 2
        }

    else
        { ufos | waitStep = ufos.waitStep - 1 }


evalStep : ( GameState, List Sound ) -> ( GameState, List Sound )
evalStep ( gameState, sounds ) =
    let
        ufoTickSkip =
            20

        getStepSound { steps, waitStep } =
            case modBy 4 steps of
                0 ->
                    Sound.UfoStep1

                1 ->
                    Sound.UfoStep2

                2 ->
                    Sound.UfoStep3

                _ ->
                    Sound.UfoStep4
    in
    ( { gameState
        | ufos = stepUfos gameState.ufos
        , lasers = stepLasers gameState.lasers
      }
    , if gameState.ufos.waitStep == 0 then
        getStepSound gameState.ufos :: sounds

      else
        sounds
    )


evalHits : ( GameState, List Sound ) -> ( GameState, List Sound )
evalHits ( { lasers, ufos } as gameState, sounds ) =
    let
        ( laserHits, ufoHits ) =
            calcHits lasers.list ufos.list
                |> List.unzip

        hits =
            List.length ufoHits
    in
    ( { gameState
        | lasers = { lasers | list = subtractList lasers.list laserHits }
        , ufos = { ufos | list = subtractList ufos.list ufoHits }
        , score = hits * 10 + gameState.score
      }
    , if hits > 0 then
        Sound.Hit :: sounds

      else
        sounds
    )


evalResults : ( GameState, List Sound ) -> ( AppPhase, List Sound )
evalResults ( gameState, sounds ) =
    if gameState.startPosition >= 120 then
        ( Congrats, sounds )

    else if List.length gameState.ufos.list == 0 then
        ( Playing
            { gameState
                | ufos = initUfos (gameState.startPosition + 30)
                , startPosition = gameState.startPosition + 30
            }
        , sounds
        )

    else if List.foldl (\ufo lowest -> max ufo.y lowest) 0 gameState.ufos.list < 190 then
        ( Playing gameState, sounds )

    else
        ( GameOver, Sound.Explode :: sounds )


calcHits : List Laser -> List Ufo -> List ( Laser, Ufo )
calcHits lasers ufos =
    let
        isHit laser ufo =
            (ufo.x <= laser.x && laser.x <= ufo.x + 10)
                && (ufo.y - 10 < laser.y && laser.y < ufo.y + 10)
    in
    doubleFoldl
        (\laser ufo hits ->
            if isHit laser ufo then
                ( laser, ufo ) :: hits

            else
                hits
        )
        lasers
        ufos


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Browser.Events.onKeyDown (decodeKeyPress KeyDown)
        , Browser.Events.onKeyUp (decodeKeyPress KeyUp)
        , Time.every 50 Tick
        ]



-- Helpers


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
