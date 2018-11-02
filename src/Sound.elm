port module Sound exposing (Sound(..), play, soundEffect)


port soundEffect : String -> Cmd msg


type Sound
    = Fire
    | Hit
    | Explode
    | UfoStep1
    | UfoStep2
    | UfoStep3
    | UfoStep4


play : List Sound -> Cmd msg
play sounds =
    List.map (soundToString >> soundEffect) sounds |> Cmd.batch


soundToString : Sound -> String
soundToString sound =
    case sound of
        Fire ->
            "fire"

        Hit ->
            "hit"

        Explode ->
            "explode"

        UfoStep1 ->
            "ufoStep1"

        UfoStep2 ->
            "ufoStep2"

        UfoStep3 ->
            "ufoStep3"

        UfoStep4 ->
            "ufoStep4"
