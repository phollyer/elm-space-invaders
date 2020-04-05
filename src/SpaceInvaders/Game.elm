module SpaceInvaders.Game exposing
    ( Model
    , Flags, init
    , Msg, update
    , subscriptions
    , view
    )

{-| Entry point for Space Invaders Game.


# Model

@docs Model


# INITIALIZING

@docs Flags, init


# UPDATING

@docs Msg, update


# SUBSCRIBING

@docs subscriptions


# VIEWING

@docs view

-}

import Element exposing (Element)
import ResponsiveUI as RUI exposing (MaxWidth, MinWidth)
import Shared.Level as Level exposing (Difficulty(..))
import Shared.Players as Players exposing (Highscores(..), Lives(..))
import SpaceInvaders.Controls.Controls as Controls
import SpaceInvaders.Logic as Logic exposing (GameData)
import SpaceInvaders.Screens.InPlay as InPlay
import SpaceInvaders.Screens.Start as StartScreen



-- MODEL


{-| -}
type Model
    = StartScreen StartScreen.Model RUI.Model GameData
    | InPlay RUI.Model GameData



-- INITIALIZING


{-| -}
type alias Flags =
    { viewport :
        { height : Float
        , width : Float
        }
    , highscores :
        List
            { name : String
            , points : Int
            }
    }


{-| -}
init : Flags -> Model
init flags =
    let
        players =
            Players.init
                (Lives 3)
                (Highscores
                    { list = flags.highscores
                    , max = 10
                    }
                )
    in
    StartScreen
        (StartScreen.init
            Easy
            players
        )
        (RUI.init
            576
            1440
            (flags
                |> .viewport
            )
        )
        (Logic.init
            Level.init
            Easy
            players
        )



-- UPDATING


{-| -}
type Msg
    = StartScreenMsg StartScreen.Msg
    | InPlayMsg Controls.Touch
    | LogicMsg Logic.Msg
    | ResponsiveMsg RUI.Msg


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( StartScreenMsg startMsg, StartScreen subModel viewport gameData ) ->
            let
                ( startModel, cmd ) =
                    subModel
                        |> StartScreen.update
                            startMsg
            in
            case startModel |> StartScreen.start of
                True ->
                    ( InPlay
                        viewport
                        (startModel
                            |> StartScreen.players
                            |> Logic.init
                                Level.init
                                (startModel
                                    |> StartScreen.difficulty
                                )
                        )
                    , cmd
                        |> Cmd.map
                            StartScreenMsg
                    )

                False ->
                    ( StartScreen
                        startModel
                        viewport
                        gameData
                    , cmd
                        |> Cmd.map
                            StartScreenMsg
                    )

        ( InPlayMsg inPlayMsg, InPlay viewport gameData ) ->
            let
                ( gameUpdated, cmd ) =
                    gameData
                        |> Logic.updateTouch
                            inPlayMsg
            in
            case gameUpdated |> Logic.state of
                Logic.Quit ->
                    ( gameUpdated, cmd )
                        |> start
                            viewport

                _ ->
                    ( InPlay
                        viewport
                        gameUpdated
                    , cmd
                        |> Cmd.map
                            LogicMsg
                    )

        ( LogicMsg gameMsg, InPlay viewport gameData ) ->
            let
                ( gameUpdated, cmd ) =
                    gameData
                        |> Logic.update
                            gameMsg
            in
            case gameUpdated |> Logic.state of
                Logic.AllGamesOver ->
                    ( gameUpdated, cmd )
                        |> start
                            viewport

                Logic.Quit ->
                    ( gameUpdated, cmd )
                        |> start
                            viewport

                _ ->
                    ( InPlay viewport gameUpdated
                    , cmd
                        |> Cmd.map
                            LogicMsg
                    )

        ( ResponsiveMsg ruiMsg, _ ) ->
            let
                rui_ =
                    model
                        |> rui
                        |> RUI.update
                            ruiMsg
            in
            case model of
                StartScreen subModel _ gameData ->
                    ( StartScreen
                        subModel
                        rui_
                        gameData
                    , Cmd.none
                    )

                InPlay _ gameData ->
                    ( InPlay
                        rui_
                        gameData
                    , Cmd.none
                    )

        _ ->
            ( model
            , Cmd.none
            )


rui : Model -> RUI.Model
rui model =
    case model of
        StartScreen _ rui_ _ ->
            rui_

        InPlay rui_ _ ->
            rui_


start : RUI.Model -> ( GameData, Cmd Logic.Msg ) -> ( Model, Cmd Msg )
start rui_ ( gameData, cmd ) =
    ( StartScreen
        (StartScreen.init
            (gameData
                |> Logic.difficulty
            )
            (gameData
                |> Logic.players
            )
        )
        rui_
        gameData
    , cmd
        |> Cmd.map
            LogicMsg
    )



-- VIEW


{-| -}
view : Model -> Element Msg
view model =
    case model of
        StartScreen subModel rui_ _ ->
            subModel
                |> StartScreen.view
                    (rui_
                        |> RUI.viewport
                    )
                |> Element.map
                    StartScreenMsg

        InPlay rui_ gameData ->
            gameData
                |> InPlay.view
                    (rui_
                        |> RUI.viewport
                    )
                |> Element.map
                    InPlayMsg



--  SUBSCRIPTIONS


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        StartScreen _ _ _ ->
            RUI.subscriptions
                |> Sub.map
                    ResponsiveMsg

        InPlay _ gameData ->
            [ RUI.subscriptions
                |> Sub.map
                    ResponsiveMsg
            , gameData
                |> Logic.subscriptions
                |> Sub.map
                    LogicMsg
            ]
                |> Sub.batch
