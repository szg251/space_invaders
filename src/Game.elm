module Game exposing
    ( AppPhase(..)
    , GameState
    , Laser
    , Ufo
    , UserControl(..)
    , calcHits
    , evalHits
    , evalResults
    , evalStep
    , evalUserControl
    , init
    )

-- Game constants


laserFrequency : Int
laserFrequency =
    8


laserSpeed : Int
laserSpeed =
    2


type AppPhase
    = Playing GameState
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
    , activeControls : List UserControl
    , startPosition : Int
    , score : Int
    }


init : GameState
init =
    { shipXPosition = 135
    , ufos = initUfos 0
    , lasers = { list = [], waitStep = 0 }
    , activeControls = []
    , startPosition = 0
    , score = 0
    }


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
                if gameState.lasers.waitStep == 0 then
                    { state | lasers = shootLaser (state.shipXPosition + 5) state.lasers }

                else
                    state

            else
                state
    in
    gameState
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


evalStep : GameState -> GameState
evalStep gameState =
    let
        ufoTickSkip =
            20
    in
    { gameState
        | ufos = stepUfos gameState.ufos
        , lasers = stepLasers gameState.lasers
    }


evalHits : GameState -> GameState
evalHits ({ lasers, ufos } as gameState) =
    let
        ( laserHits, ufoHits ) =
            calcHits lasers.list ufos.list
                |> List.unzip
    in
    { gameState
        | lasers = { lasers | list = subtractList lasers.list laserHits }
        , ufos = { ufos | list = subtractList ufos.list ufoHits }
        , score = List.length ufoHits * 10 + gameState.score
    }


evalResults : GameState -> AppPhase
evalResults gameState =
    if gameState.startPosition >= 120 then
        Congrats

    else if List.length gameState.ufos.list == 0 then
        Playing
            { gameState
                | ufos = initUfos (gameState.startPosition + 30)
                , startPosition = gameState.startPosition + 30
            }

    else if List.foldl (\ufo lowest -> max ufo.y lowest) 0 gameState.ufos.list < 190 then
        Playing gameState

    else
        GameOver


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



-- Helpers


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
