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
import Http
import ResponsiveUI as RUI
import Shared.Level as Level exposing (Difficulty(..))
import Shared.Players as Players exposing (Highscores(..), Lives(..))
import SpaceInvaders.Controls.Controls as Controls
import SpaceInvaders.Highscores as Highscores
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
    , cache :
        { readEndpoint : String
        , writeEndpoint : String
        , anonKey : String
        }
    }


{-| -}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        cacheConfig =
            { readEndpoint = flags.cache.readEndpoint
            , writeEndpoint = flags.cache.writeEndpoint
            , anonKey = flags.cache.anonKey
            }

        players =
            Players.init
                (Lives 3)
                (Highscores
                    { list = []
                    , max = 10
                    }
                )
    in
    ( StartScreen
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
            cacheConfig
            players
        )
    , Highscores.loadHighscores
        cacheConfig
        HighscoresLoaded
    )



-- UPDATING


{-| -}
type Msg
    = StartScreenMsg StartScreen.Msg
    | InPlayMsg Controls.Touch
    | LogicMsg Logic.Msg
    | ResponsiveMsg RUI.Msg
    | HighscoresLoaded (Result Http.Error (List { name : String, points : Int }))


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( StartScreenMsg startMsg, StartScreen subModel viewport gameData ) ->
            let
                ( startModel, cmd ) =
                    StartScreen.update startMsg subModel
            in
            if StartScreen.start startModel then
                ( InPlay
                    viewport
                    (Logic.init Level.init
                        (StartScreen.difficulty startModel)
                        (Logic.cacheConfig gameData)
                        (StartScreen.players startModel)
                    )
                , Cmd.map StartScreenMsg cmd
                )

            else
                ( StartScreen startModel viewport gameData
                , Cmd.map StartScreenMsg cmd
                )

        ( InPlayMsg inPlayMsg, InPlay viewport gameData ) ->
            let
                ( gameUpdated, cmd ) =
                    Logic.updateTouch inPlayMsg gameData
            in
            case Logic.state gameUpdated of
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

        ( HighscoresLoaded (Ok loadedHighscores), StartScreen startModel viewport gameData ) ->
            let
                difficulty_ =
                    startModel
                        |> StartScreen.difficulty

                players_ =
                    Players.init
                        (Lives 3)
                        (Highscores
                            { list = loadedHighscores
                            , max = 10
                            }
                        )
            in
            ( StartScreen
                (StartScreen.init
                    difficulty_
                    players_
                )
                viewport
                (Logic.init
                    Level.init
                    difficulty_
                    (Logic.cacheConfig gameData)
                    players_
                )
            , Cmd.none
            )

        ( HighscoresLoaded (Err err), _ ) ->
            case model of
                StartScreen startModel viewport gameData ->
                    ( StartScreen
                        startModel
                        viewport
                        (gameData
                            |> Logic.setCacheError
                                (Just ("Could not load highscores from backend: " ++ httpErrorToString err))
                        )
                    , Cmd.none
                    )

                InPlay viewport gameData ->
                    ( InPlay
                        viewport
                        (gameData
                            |> Logic.setCacheError
                                (Just ("Could not load highscores from backend: " ++ httpErrorToString err))
                        )
                    , Cmd.none
                    )

        ( HighscoresLoaded (Ok _), InPlay _ _ ) ->
            ( model
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
    , Cmd.batch
        [ cmd
            |> Cmd.map
                LogicMsg
        , Highscores.loadHighscores
            (gameData
                |> Logic.cacheConfig
            )
            HighscoresLoaded
        ]
    )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "bad URL: " ++ url

        Http.Timeout ->
            "request timed out"

        Http.NetworkError ->
            "network error"

        Http.BadStatus statusCode ->
            "HTTP " ++ String.fromInt statusCode

        Http.BadBody message ->
            "bad response body: " ++ message



-- VIEW


{-| -}
view : Model -> Element Msg
view model =
    case model of
        StartScreen subModel rui_ gameData ->
            subModel
                |> StartScreen.view
                    (rui_
                        |> RUI.viewport
                    )
                |> Element.map
                    StartScreenMsg
                |> withCacheError
                    (gameData
                        |> Logic.cacheError
                    )

        InPlay rui_ gameData ->
            gameData
                |> InPlay.view
                    (rui_
                        |> RUI.viewport
                    )
                |> Element.map
                    InPlayMsg
                |> withCacheError
                    (gameData
                        |> Logic.cacheError
                    )


withCacheError : Maybe String -> Element Msg -> Element Msg
withCacheError maybeError content =
    case maybeError of
        Nothing ->
            content

        Just errorMessage ->
            Element.column
                [ Element.width Element.fill ]
                [ Element.el
                    [ Element.padding 8 ]
                    (Element.text ("Backend error: " ++ errorMessage))
                , content
                ]



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
