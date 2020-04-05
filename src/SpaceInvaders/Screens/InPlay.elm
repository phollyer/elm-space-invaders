module SpaceInvaders.Screens.InPlay exposing (view)

{-| The view for playing the game.

@docs view

-}

import Color.Black exposing (black, grey)
import Color.Grey exposing (lightgrey)
import Color.White exposing (white)
import Color.Yellow exposing (yellow)
import Element as El exposing (DeviceClass(..), Element, Orientation(..), alignLeft, alignRight, centerX, centerY, el, fill, height, html, inFront, padding, paddingXY, row, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Events.Extra.Touch as Touch
import ResponsiveUI as RUI exposing (DefaultView(..), Option(..), Viewport)
import ResponsiveUI.Element as Rel
import ResponsiveUI.Element.Font as RFont
import Shared.Players as Players exposing (Player, Players)
import SpaceInvaders.Assets.Aliens as Aliens
import SpaceInvaders.Assets.Bunkers as Bunkers
import SpaceInvaders.Assets.Ground as Ground
import SpaceInvaders.Assets.Laser as Laser
import SpaceInvaders.Assets.Lasers as Lasers
import SpaceInvaders.Assets.Mothership as Mothership
import SpaceInvaders.Assets.Ship as Ship exposing (Color(..), Ship)
import SpaceInvaders.Configs as Configs
import SpaceInvaders.Controls.Controls as Controls
import SpaceInvaders.Logic as Logic exposing (GameData, GameState(..))
import SpaceInvaders.PlayerState as PlayerState
import Svg exposing (Svg, svg)
import Svg.Attributes as SA exposing (viewBox)



-- VIEW


{-| -}
view : Viewport -> GameData -> Element Controls.Touch
view viewport game =
    El.el
        [ Background.color lightgrey
        , Border.glow grey 10
        , Border.color white
        , Border.width 2
        , height fill
        , width fill
        ]
        (RUI.select
            (DefaultView
                (desktopView
                    viewport
                    game
                )
            )
            [ TouchPortrait
                (touchPortraitView
                    viewport
                    game
                )
            , TouchLandscape
                (touchLandscapeView
                    viewport
                    game
                )
            ]
            viewport
        )


config : Configs.View
config =
    Configs.view



-- DESKTOP


desktopView : Viewport -> GameData -> Element Controls.Touch
desktopView viewport game =
    El.column
        [ El.centerX
        , El.centerY
        ]
        [ game
            |> gameView
                viewport
        ]



-- TOUCH


touchLandscapeView : Viewport -> GameData -> Element Controls.Touch
touchLandscapeView viewport game =
    El.row
        [ El.centerX
        , El.centerY
        ]
        [ touchControlsLeft
            game
        , game
            |> gameView
                viewport
        , touchControlsRight
            game
        ]


touchPortraitView : Viewport -> GameData -> Element Controls.Touch
touchPortraitView viewport game =
    El.column
        [ El.centerX
        , El.centerY
        ]
        [ gameView
            viewport
            game
        , touchControlsPortraitView
            viewport
            game
        ]



-- GAME


gameView : Viewport -> GameData -> Element Controls.Touch
gameView viewport game =
    let
        ( width, height ) =
            scaleGameView
                viewport
    in
    El.el
        [ Background.color black
        , El.height
            (height
                |> round
                |> El.px
            )
        , El.width
            (width
                |> round
                |> El.px
            )
        , inFront
            (game
                |> Logic.players
                |> topLine
                    viewport
            )
        , inFront
            (game
                |> Logic.state
                |> playerMessage
                    viewport
            )
        ]
        (html <|
            svg
                [ viewBox config.viewBox
                , SA.height
                    (height |> String.fromFloat)
                , SA.width
                    (width |> String.fromFloat)
                ]
                (Aliens.view
                    (game
                        |> Logic.aliens
                    )
                    ++ Lasers.view
                        (game
                            |> Logic.mothershipLasers
                        )
                    ++ Lasers.view
                        (game
                            |> Logic.alienLasers
                        )
                    ++ [ Mothership.view
                            (game
                                |> Logic.mothership
                            )
                       ]
                    ++ Bunkers.view
                        (game
                            |> Logic.bunkers
                        )
                    ++ [ Laser.view
                            (game
                                |> Logic.laser
                            )
                       ]
                    ++ [ game
                            |> Logic.state
                            |> ship
                                (game
                                    |> Logic.ship
                                )
                       ]
                    ++ [ Ground.grass
                            width
                       ]
                    ++ [ Ground.soil
                            width
                       ]
                    ++ (game
                            |> Logic.state
                            |> spareShips
                                (game
                                    |> Logic.players
                                )
                       )
                )
        )


scaleGameView : Viewport -> ( Float, Float )
scaleGameView viewport =
    let
        ( gameWidth, gameHeight ) =
            RUI.aspectRatio
                3
                2
                viewport
    in
    case ( viewport |> RUI.class, viewport |> RUI.orientation ) of
        ( Desktop, _ ) ->
            ( gameWidth, gameHeight )

        ( BigDesktop, _ ) ->
            ( gameWidth, gameHeight )

        ( _, Portrait ) ->
            ( gameWidth, gameHeight )

        ( _, Landscape ) ->
            let
                viewportWidth =
                    viewport
                        |> RUI.viewportWidth

                viewportHeight =
                    viewport
                        |> RUI.viewportHeight

                width =
                    case gameWidth < (viewportWidth - 100) of
                        True ->
                            gameWidth

                        False ->
                            viewportWidth - 100

                height =
                    width / 3 * 2
            in
            ( width, height )


topLine : Viewport -> Players PlayerState.State -> Element Controls.Touch
topLine viewport players =
    let
        padding_ =
            Rel.paddingXYWithOptions
                2
                2
                [ AllTablet ( 5, 5 )
                , AllDesktop ( 5, 5 )
                , AllBigDesktop ( 5, 10 )
                ]
                viewport
    in
    row
        [ Font.color yellow
        , RFont.sizeWithOptions
            10
            [ AllLandscape 1.2 ]
            viewport
        , height shrink
        , width fill
        , inFront
            (el
                [ centerX
                , padding_
                ]
                (text
                    ("Highscore "
                        ++ (players
                                |> Players.highscoreToString
                           )
                    )
                )
            )
        ]
        [ el
            [ alignLeft
            , padding_
            ]
            (text
                (players
                    |> playerText 1
                )
            )
        , el
            [ alignRight
            , padding_
            ]
            (text
                (players
                    |> playerText 2
                )
            )
        ]


playerMessage : Viewport -> GameState -> Element msg
playerMessage viewport state =
    let
        message =
            case state of
                IntroducingPlayer num ->
                    "Player "
                        ++ (num
                                |> String.fromInt
                           )

                GameOver ->
                    "Game Over"

                _ ->
                    ""
    in
    case message of
        "" ->
            El.none

        _ ->
            el
                [ Font.size
                    40
                , Font.color
                    yellow
                , centerX
                , centerY
                ]
                (text message)


playerText : Int -> Players PlayerState.State -> String
playerText index players =
    case index <= (players |> Players.total) of
        True ->
            let
                name =
                    players
                        |> Players.nameForPlayer
                            index
                        |> Maybe.withDefault
                            "Not Found"

                points =
                    players
                        |> Players.pointsForPlayer
                            index
                        |> Maybe.withDefault
                            0
                        |> String.fromInt
            in
            "Player " ++ (index |> String.fromInt) ++ ": (" ++ name ++ ") : Score " ++ points

        False ->
            ""


ship : Ship -> GameState -> Svg msg
ship ship_ state =
    case state of
        IntroducingPlayer _ ->
            ship_
                |> Ship.updateColor
                    Yellow
                |> Ship.view

        _ ->
            ship_
                |> Ship.view


spareShips : Players PlayerState.State -> GameState -> List (Svg msg)
spareShips players state =
    let
        current =
            players
                |> Players.current
                |> Players.number
    in
    players
        |> Players.all
        |> List.indexedMap
            (spareLives state current)
        |> List.concat


spareLives : GameState -> Int -> Int -> Player PlayerState.State -> List (Svg msg)
spareLives state current index player =
    let
        playerNumber =
            index + 1

        lives =
            case current == playerNumber of
                True ->
                    (player
                        |> Players.lives
                    )
                        - 1

                False ->
                    player
                        |> Players.lives

        color =
            case state of
                IntroducingPlayer num ->
                    case num == playerNumber of
                        True ->
                            Yellow

                        False ->
                            White

                _ ->
                    White
    in
    List.range
        1
        lives
        |> List.map
            (Ship.spareLife color playerNumber)
        |> List.map
            Ship.view



-- TOUCH CONTROLS


touchControlsPortraitView : Viewport -> GameData -> Element Controls.Touch
touchControlsPortraitView viewport game =
    let
        totalWidth =
            viewport
                |> RUI.viewportWidth

        controlsWidth =
            totalWidth / 3

        btnSpacing =
            30

        btnWidth =
            (controlsWidth - btnSpacing) / 2
    in
    El.row
        [ width fill
        , El.spaceEvenly
        , El.paddingXY
            0
            20
        ]
        [ El.row
            [ width fill
            , El.alignTop
            ]
            [ El.row
                [ El.spacing
                    btnSpacing
                ]
                [ El.el
                    []
                    (Controls.touchLeft
                        btnWidth
                    )
                , El.el
                    []
                    (Controls.touchFire
                        btnWidth
                    )
                ]
            ]
        , El.row
            [ width fill
            ]
            [ El.column
                [ El.centerY
                , El.centerX
                ]
                [ El.el
                    [ El.centerY
                    ]
                    (case game |> Logic.state of
                        Pause ->
                            Controls.touchResume
                                btnWidth

                        _ ->
                            Controls.touchPause
                                btnWidth
                    )
                , Controls.touchQuit
                ]
            ]
        , El.row
            [ width fill
            , El.alignTop
            ]
            [ El.row
                [ El.alignRight
                , El.spacing
                    btnSpacing
                ]
                [ El.el
                    []
                    (Controls.touchFire
                        btnWidth
                    )
                , El.el
                    []
                    (Controls.touchRight
                        btnWidth
                    )
                ]
            ]
        ]



-- TOUCH CONTROLS LANDSCAPE


touchControlsLeft : GameData -> Element Controls.Touch
touchControlsLeft game =
    El.column
        [ El.spacing 30 ]
        [ El.el
            []
            Controls.touchQuit
        , El.el
            []
            (case game |> Logic.state of
                Pause ->
                    Controls.touchResume
                        50

                _ ->
                    Controls.touchPause
                        50
            )
        , El.el
            []
            (Controls.touchFire
                50
            )
        , El.el
            []
            (Controls.touchLeft
                50
            )
        ]


touchControlsRight : GameData -> Element Controls.Touch
touchControlsRight game =
    El.column
        [ El.spacing 30 ]
        [ El.el
            []
            Controls.touchQuit
        , El.el
            []
            (case game |> Logic.state of
                Pause ->
                    Controls.touchResume
                        50

                _ ->
                    Controls.touchPause
                        50
            )
        , El.el
            []
            (Controls.touchFire
                50
            )
        , El.el
            []
            (Controls.touchRight
                50
            )
        ]
