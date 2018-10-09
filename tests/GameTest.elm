module GameTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Game exposing (GameState, UserControl(..))
import Test exposing (..)


initGS : GameState
initGS =
    Game.init


suite : Test
suite =
    describe "Game module "
        [ describe "evaluating user control"
            [ test "move the ship left" <|
                \_ ->
                    let
                        before =
                            { initGS
                                | shipXPosition = 135
                                , activeControls = [ Left ]
                            }

                        after =
                            { before | shipXPosition = 130 }
                    in
                    Expect.equal
                        (Game.evalUserControl before)
                        after
            , test "move the ship right" <|
                \_ ->
                    let
                        before =
                            { initGS
                                | shipXPosition = 135
                                , activeControls = [ Right ]
                            }

                        after =
                            { before | shipXPosition = 140 }
                    in
                    Expect.equal
                        (Game.evalUserControl before)
                        after
            , test "fire a laser (from the ship's position)" <|
                \_ ->
                    let
                        before =
                            { initGS
                                | shipXPosition = 135
                                , activeControls = [ Fire ]
                            }

                        after =
                            { before
                                | lasers = [ { x = 140, y = 180 } ]
                            }
                    in
                    Expect.equal
                        (Game.evalUserControl before)
                        after
            , test "move the ship left and fire a laser" <|
                \_ ->
                    let
                        before =
                            { initGS
                                | shipXPosition = 135
                                , activeControls = [ Left, Fire ]
                            }

                        after =
                            { before
                                | shipXPosition = 130
                                , lasers = [ { x = 135, y = 180 } ]
                            }
                    in
                    Expect.equal
                        (Game.evalUserControl before)
                        after
            ]
        , describe "evaluating hits"
            [ test "hit in the middle" <|
                \_ ->
                    let
                        before =
                            { initGS
                                | ufos = [ { x = 10, y = 10 } ]
                                , lasers = [ { x = 15, y = 10 } ]
                            }

                        after =
                            { before | ufos = [], lasers = [] }
                    in
                    Expect.equal
                        (Game.evalHits before)
                        after
            , test "hit the left bottom corner" <|
                \_ ->
                    let
                        before =
                            { initGS
                                | ufos = [ { x = 10, y = 10 } ]
                                , lasers = [ { x = 10, y = 19 } ]
                            }

                        after =
                            { before | ufos = [], lasers = [] }
                    in
                    Expect.equal
                        (Game.evalHits before)
                        after
            , test "hit the left upper corner" <|
                \_ ->
                    let
                        before =
                            { initGS
                                | ufos = [ { x = 10, y = 10 } ]
                                , lasers = [ { x = 10, y = 1 } ]
                            }

                        after =
                            { before | ufos = [], lasers = [] }
                    in
                    Expect.equal
                        (Game.evalHits before)
                        after
            , test "hit the right bottom corner" <|
                \_ ->
                    let
                        before =
                            { initGS
                                | ufos = [ { x = 10, y = 10 } ]
                                , lasers = [ { x = 20, y = 19 } ]
                            }

                        after =
                            { before | ufos = [], lasers = [] }
                    in
                    Expect.equal
                        (Game.evalHits before)
                        after
            , test "hit the right upper corner" <|
                \_ ->
                    let
                        before =
                            { initGS
                                | ufos = [ { x = 10, y = 10 } ]
                                , lasers = [ { x = 20, y = 1 } ]
                            }

                        after =
                            { before | ufos = [], lasers = [] }
                    in
                    Expect.equal
                        (Game.evalHits before)
                        after
            ]
        ]
