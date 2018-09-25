module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Icons
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg, g, svg)
import Svg.Attributes exposing (enableBackground, height, viewBox, width)
import Time


type alias Model =
    { shipX : Int
    , ufos : List Ufo
    , steps : Int
    }


type alias Ufo =
    { x : Int
    , y : Int
    , alive : Bool
    }


type Direction
    = Left
    | Right


init : () -> ( Model, Cmd Msg )
init _ =
    ( { shipX = 0
      , ufos = initUfos
      , steps = 1
      }
    , Cmd.none
    )


initUfos : List Ufo
initUfos =
    let
        xs =
            List.range 1 10
                |> List.map ((*) 20)

        ys =
            List.range 1 5
                |> List.map ((*) 20)
    in
    List.concatMap (\x -> List.map (\y -> Ufo x y True) ys) xs


type Msg
    = KeyPress Direction
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress key ->
            case key of
                Right ->
                    ( { model | shipX = model.shipX + 5 }, Cmd.none )

                Left ->
                    ( { model | shipX = model.shipX - 5 }, Cmd.none )

        Tick _ ->
            ( { model
                | steps = model.steps + 1
                , ufos = List.map (stepUfo model.steps) model.ufos
              }
            , Cmd.none
            )


stepUfo : Int -> Ufo -> Ufo
stepUfo steps ufo =
    let
        stepRem =
            remainderBy 20 steps
    in
    if stepRem == 0 || stepRem == 10 then
        { ufo | y = ufo.y + 10 }

    else if 10 < stepRem then
        { ufo | x = ufo.x - 10 }

    else
        { ufo | x = ufo.x + 10 }


view : Model -> Html Msg
view model =
    div
        []
        [ viewGamePanel model ]


viewGamePanel : Model -> Svg Msg
viewGamePanel model =
    svg
        [ viewBox "0 0 400 300"
        , width "1000"
        , height "500"
        , style "backgroundColor" "black"
        ]
        [ g [] (List.concat [ viewUfos model.ufos, [ Icons.viewShip model.shipX ] ])
        ]


viewUfos : List Ufo -> List (Svg Msg)
viewUfos =
    List.filter .alive >> List.map Icons.viewUfo


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown decodeKeyPress
        , Time.every 1000 Tick
        ]


decodeKeyPress : Decoder Msg
decodeKeyPress =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToDirection
        |> Decode.map KeyPress


keyToDirection : String -> Decoder Direction
keyToDirection key =
    case key of
        "ArrowRight" ->
            Decode.succeed Right

        "ArrowLeft" ->
            Decode.succeed Left

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
