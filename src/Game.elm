module Game exposing
    ( AppPhase(..)
    , GameState
    , Laser
    , Ufo
    , UserControl(..)
    , calcHits
    , evalHits
    , evalResults
    , evalUfoStep
    , evalUserControl
    , init
    )


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


type alias GameState =
    { shipXPosition : Int
    , ufos : List Ufo
    , steps : Int
    , lasers : List Laser
    , activeControls : List UserControl
    }


init : GameState
init =
    { shipXPosition = 135
    , ufos = initUfos
    , steps = 0
    , lasers = []
    , activeControls = []
    }


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
    { gameState
        | lasers = subtractList lasers laserHits
        , ufos = subtractList ufos ufoHits
    }


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
