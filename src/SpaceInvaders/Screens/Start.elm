module SpaceInvaders.Screens.Start exposing
    ( Model
    , init
    , Msg, update
    , start, players, difficulty
    , view
    )

{-| The start screen.

@docs Model


# INITIALIZING

@docs init


# UPDATING

@docs Msg, update


# QUERYING

@docs start, players, difficulty


# VIEWING

@docs view

-}

import Browser.Dom as Dom
import Color.Black exposing (black)
import Color.Blue exposing (blue, lightblue)
import Color.Yellow exposing (lightyellow, yellow)
import Element as El exposing (Attribute, Element, fill, fillPortion, inFront)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Extras.List as List
import Extras.String as String
import Html.Attributes as Attr
import ResponsiveUI as RUI exposing (DefaultView(..), Option(..), Viewport)
import ResponsiveUI.Element as Rel
import ResponsiveUI.Element.Font as RFont
import Shared.Level exposing (Difficulty(..))
import Shared.Players as Players exposing (Highscores(..), Lives(..), Players)
import Shared.Scores as Scores exposing (Scores)
import SpaceInvaders.Controls.Touch.Fire as TouchFire
import SpaceInvaders.Controls.Touch.Left as TouchLeft
import SpaceInvaders.Controls.Touch.Right as TouchRight
import SpaceInvaders.PlayerState exposing (State)
import SpaceInvaders.SFX as SFX exposing (Sound(..))
import Task



-- MODEL


{-| A type representing the data the page requires in order to function.

This is an opaque type, use the exposed API to interact with it.

-}
type Model
    = Model
        { start : Bool
        , enteringNames : Bool
        , difficulty : Difficulty
        , players : Players State
        }



-- INITIALIZING


{-| -}
init : Difficulty -> Players State -> Model
init difficulty_ players_ =
    Model
        { start = False
        , enteringNames = False
        , players = players_
        , difficulty = difficulty_
        }



-- UPDATING


{-| -}
type Msg
    = SelectDifficulty Difficulty
    | SelectPlayers Int
    | EnterPlayersNames
    | CancelNameEntry
    | NameEntered Int String
    | StartGame
    | NoOp


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( model
                |> updateStart
                    True
            , SFX.init
                |> SFX.add
                    HitPlayer
                |> SFX.execute
            )

        SelectDifficulty difficulty_ ->
            ( model
                |> updateDifficulty
                    difficulty_
            , SFX.init
                |> SFX.add
                    HitMothership
                |> SFX.execute
            )

        SelectPlayers num ->
            ( model
                |> updateEnteringNames
                    True
                |> updatePlayers
                    (model
                        |> players
                        |> register
                            num
                    )
            , [ SFX.init
                    |> SFX.add
                        Fire
                    |> SFX.execute
              , Task.perform
                    (\_ -> NoOp)
                    (Dom.setViewport 0 0)
              , Task.attempt
                    (\_ -> NoOp)
                    (Dom.focus "player-1-input")
              ]
                |> Cmd.batch
            )

        EnterPlayersNames ->
            ( model
                |> updateEnteringNames
                    True
                |> updatePlayers
                    (model
                        |> players
                        |> register
                            (model
                                |> players
                                |> Players.total
                            )
                    )
            , [ SFX.init
                    |> SFX.add
                        HitAlien
                    |> SFX.execute
              , Task.perform
                    (\_ -> NoOp)
                    (Dom.setViewport 0 0)
              , Task.attempt
                    (\_ -> NoOp)
                    (Dom.focus "player-1-input")
              ]
                |> Cmd.batch
            )

        CancelNameEntry ->
            ( model
                |> updateEnteringNames
                    False
            , SFX.init
                |> SFX.add
                    HitBunker
                |> SFX.execute
            )

        NameEntered index name ->
            ( model
                |> updatePlayers
                    (model
                        |> players
                        |> Players.updateNameForPlayer
                            index
                            name
                    )
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


register : Int -> Players State -> Players State
register num players_ =
    let
        registered =
            players_
                |> Players.removeAll
                |> Players.register
                    ""
    in
    case num of
        2 ->
            registered
                |> Players.register
                    ""

        _ ->
            registered



-- QUERYING


{-| -}
difficulty : Model -> Difficulty
difficulty (Model model) =
    model.difficulty


enteringNames : Model -> Bool
enteringNames (Model model) =
    model.enteringNames


{-| -}
players : Model -> Players State
players (Model model) =
    model.players


{-| Flag to signify if the game should be started or not.
-}
start : Model -> Bool
start (Model model) =
    model.start



-- UPDATING


updateDifficulty : Difficulty -> Model -> Model
updateDifficulty difficulty_ (Model model) =
    Model
        { model
            | difficulty = difficulty_
        }


updateEnteringNames : Bool -> Model -> Model
updateEnteringNames enteringNames_ (Model model) =
    Model
        { model
            | enteringNames = enteringNames_
        }


updatePlayers : Players State -> Model -> Model
updatePlayers players_ (Model model) =
    Model
        { model
            | players = players_
        }


updateStart : Bool -> Model -> Model
updateStart start_ (Model model) =
    Model
        { model
            | start = start_
        }



-- VIEWING


{-| -}
view : Viewport -> Model -> Element Msg
view viewport model =
    El.column
        [ Background.color black
        , Font.color yellow
        , El.height fill
        , El.width fill
        , El.paddingXY
            10
            20
        ]
        [ title
            viewport
        , highscoresOrNameEntry
            viewport
            model
        , menu
            viewport
            model
        , controlsView
            viewport
        ]



-- TITLE


title : Viewport -> Element msg
title viewport =
    El.el
        [ El.centerX
        , Font.bold
        , RFont.size
            40
            viewport
        ]
        (El.text "SPACE INVADERS")


highscoresOrNameEntry : Viewport -> Model -> Element Msg
highscoresOrNameEntry viewport model =
    let
        element =
            case model |> enteringNames of
                True ->
                    nameEntryView
                        viewport
                        (model
                            |> players
                        )

                False ->
                    listHighScores
                        viewport
                        (model
                            |> players
                            |> Players.highscores
                        )
    in
    El.el
        [ El.centerX
        , Rel.paddingXY
            0
            20
            viewport
        ]
        element



-- HIGHSCORES


listHighScores : Viewport -> Scores -> Element Msg
listHighScores viewport scores =
    let
        ( indices, scores_, names ) =
            scores
                |> Scores.list
                |> List.indexedMap
                    (\index score ->
                        ( (index + 1)
                            |> String.fromInt
                        , score
                            |> Scores.points
                            |> String.fromInt
                        , score
                            |> Scores.name
                        )
                    )
                |> List.unzip3
    in
    El.column
        [ El.centerX
        , El.paddingXY
            0
            20
        , Rel.spacing
            5
            viewport
        ]
        [ El.el
            [ Font.color yellow
            , El.centerX
            , RFont.size
                30
                viewport
            ]
            (El.text "High Scores")
        , El.row
            [ Font.color lightblue
            , El.width fill
            , Rel.spacing
                30
                viewport
            , RFont.size
                20
                viewport
            ]
            [ El.column
                []
                [ El.el
                    [ Font.color blue ]
                    (El.text "Rank")
                , El.column
                    [ El.width fill
                    , RFont.size
                        16
                        viewport
                    ]
                    (rankingNumbersList
                        indices
                    )
                ]
            , El.column
                []
                [ El.el
                    [ Font.color blue ]
                    (El.text "Score")
                , El.column
                    [ El.width fill
                    , RFont.size
                        16
                        viewport
                    ]
                    (scoresList
                        scores_
                    )
                ]
            , El.column
                []
                [ El.el
                    [ Font.color blue
                    , El.centerX
                    ]
                    (El.text "Name")
                , El.column
                    [ El.width fill
                    , RFont.size
                        16
                        viewport
                    ]
                    (namesList
                        names
                    )
                ]
            ]
        ]


rankingNumbersList : List String -> List (Element msg)
rankingNumbersList indices =
    indices
        |> List.map
            (\index ->
                El.el
                    [ El.centerX ]
                    (El.text index)
            )


scoresList : List String -> List (Element msg)
scoresList scores_ =
    scores_
        |> List.map
            (\score ->
                El.el
                    [ El.alignRight ]
                    (El.text score)
            )


namesList : List String -> List (Element msg)
namesList names =
    names
        |> List.map
            (\name ->
                El.el
                    [ El.centerX ]
                    (El.text
                        (name
                            |> String.ellipsis
                                6
                            |> String.replaceIfEmpty
                                "---"
                        )
                    )
            )



-- NAME ENTRY


nameEntryView : Viewport -> Players State -> Element Msg
nameEntryView viewport players_ =
    El.el
        [ El.centerX
        , RFont.size
            28
            viewport
        ]
        (El.column
            [ El.centerY
            , Rel.spacing
                20
                viewport
            ]
            [ El.column
                [ Rel.spacing
                    20
                    viewport
                ]
                (players_
                    |> Players.total
                    |> List.range 1
                    |> List.map
                        (nameInput
                            players_
                            viewport
                        )
                )
            , nameEntryControls
                viewport
            ]
        )


nameInput : Players State -> Viewport -> Int -> Element Msg
nameInput players_ viewport number =
    Input.text
        [ Font.color blue
        , Background.color lightblue
        , Border.rounded
            10
        , Rel.width
            300
            viewport
        , Rel.paddingXY
            5
            10
            viewport
        , RFont.size
            20
            viewport
        , El.htmlAttribute
            (Attr.id
                ("player-" ++ (number |> String.fromInt) ++ "-input")
            )
        , El.focused
            [ Background.color lightyellow ]
        ]
        { onChange = NameEntered number
        , text =
            players_
                |> Players.nameForPlayer
                    number
                |> Maybe.withDefault
                    "Anon"
        , placeholder = Just (Input.placeholder [] (El.text "Name"))
        , label =
            Input.labelAbove
                [ El.centerX
                , Rel.paddingXY
                    0
                    5
                    viewport
                , RFont.size
                    30
                    viewport
                ]
                (El.text
                    ("Player " ++ (number |> String.fromInt))
                )
        }


nameEntryControls : Viewport -> Element Msg
nameEntryControls viewport =
    El.row
        [ El.width fill
        , Rel.spacing
            10
            viewport
        ]
        [ El.el
            [ El.width fill ]
            startGameButton
        , El.el
            [ El.width fill ]
            cancelButton
        ]


startGameButton : Element Msg
startGameButton =
    El.el
        (btnAttrs
            StartGame
        )
        (El.text "Start Game")


cancelButton : Element Msg
cancelButton =
    El.el
        (El.alignRight
            :: btnAttrs
                CancelNameEntry
        )
        (El.text "Cancel")


btnAttrs : msg -> List (Attribute msg)
btnAttrs msg =
    [ Font.color blue
    , El.pointer
    , El.mouseOver
        [ Font.color lightblue ]
    , onClick
        msg
    ]



-- MENU


menu : Viewport -> Model -> Element Msg
menu viewport model =
    RUI.select
        (DefaultView
            (menuDefault
                viewport
                model
            )
        )
        [ PhonePortrait
            (menuPhone
                viewport
                model
            )
        ]
        viewport


menuPhone : Viewport -> Model -> Element Msg
menuPhone viewport model =
    El.column
        [ El.width fill
        , El.centerX
        , El.paddingXY
            0
            20
        , Rel.spacing
            30
            viewport
        ]
        [ El.row
            [ El.alignTop
            , El.spaceEvenly
            , El.width fill
            ]
            [ selectNumberOfPlayersView
                viewport
                (model
                    |> players
                    |> Players.total
                )
            , selectDifficultyView
                viewport
                (model
                    |> difficulty
                )
            ]
        , maybeStartNameEntryButton
            viewport
            (model
                |> enteringNames
            )
        ]


menuDefault : Viewport -> Model -> Element Msg
menuDefault viewport model =
    El.row
        [ El.width fill ]
        [ selectNumberOfPlayersView
            viewport
            (model
                |> players
                |> Players.total
            )
        , maybeStartNameEntryButton
            viewport
            (model
                |> enteringNames
            )
        , selectDifficultyView
            viewport
            (model
                |> difficulty
            )
        ]


maybeStartNameEntryButton : Viewport -> Bool -> Element Msg
maybeStartNameEntryButton viewport show =
    El.el
        [ El.width fill ]
        (case show of
            True ->
                El.none

            False ->
                startNameEntryButton
                    viewport
        )


startNameEntryButton : Viewport -> Element Msg
startNameEntryButton viewport =
    El.el
        ([ El.centerX
         , El.centerY
         , Font.bold
         , RFont.size
            40
            viewport
         ]
            |> List.append
                (btnAttrs
                    EnterPlayersNames
                )
        )
        (El.text "START")


selectNumberOfPlayersView : Viewport -> Int -> Element Msg
selectNumberOfPlayersView viewport numPlayers =
    let
        totalPlayers =
            case numPlayers of
                0 ->
                    1

                _ ->
                    numPlayers
    in
    El.column
        [ Font.color lightblue
        , El.alignTop
        , El.width
            (fillPortion
                5
            )
        , Rel.spacing
            10
            viewport
        , RFont.size
            22
            viewport
        ]
        [ El.el
            [ Font.color yellow
            , El.centerX
            , RFont.size
                27
                viewport
            ]
            (El.text "Players")
        , playerView
            1
            ""
            totalPlayers
        , playerView
            2
            "s"
            totalPlayers
        ]


playerView : Int -> String -> Int -> Element Msg
playerView selection append selected =
    let
        attrs =
            selected
                |> isSelected
                    selection
                    SelectPlayers
    in
    El.el
        ([ El.centerX ]
            ++ attrs
        )
        (El.text
            ((selection |> String.fromInt) ++ " Player" ++ append)
        )


selectDifficultyView : Viewport -> Difficulty -> Element Msg
selectDifficultyView viewport difficulty_ =
    El.column
        [ El.width
            (fillPortion
                5
            )
        , Rel.spacing
            10
            viewport
        , RFont.size
            22
            viewport
        ]
        [ El.el
            [ Font.color yellow
            , El.centerX
            , RFont.size
                27
                viewport
            ]
            (El.text "Difficulty")
        , difficultyView
            "Easy"
            Easy
            difficulty_
        , difficultyView
            "Medium"
            Medium
            difficulty_
        , difficultyView
            "Hard"
            Hard
            difficulty_
        ]


difficultyView : String -> Difficulty -> Difficulty -> Element Msg
difficultyView str selection selected =
    let
        attrs =
            selected
                |> isSelected
                    selection
                    SelectDifficulty
    in
    El.el
        ([ El.centerX ]
            ++ attrs
        )
        (El.text str)


isSelected : a -> (a -> Msg) -> a -> List (Attribute Msg)
isSelected selection onClickMsg selected =
    case selection == selected of
        True ->
            [ Font.color blue ]

        False ->
            [ Font.color lightblue
            , El.pointer
            , El.mouseOver
                [ Font.color yellow ]
            , onClick
                (onClickMsg
                    selection
                )
            ]



-- GAME CONTROLS


controlsView : Viewport -> Element Msg
controlsView viewport =
    El.column
        [ El.width fill
        , Rel.spacing
            20
            viewport
        , El.paddingXY
            0
            30
        , RFont.size
            20
            viewport
        ]
        [ RUI.select
            (DefaultView
                (El.column
                    [ El.width fill
                    , El.spacing
                        40
                    ]
                    [ keyboardControlsView
                        viewport
                    , touchControlsView
                        viewport
                    ]
                )
            )
            [ AllTouch
                (El.column
                    [ El.width fill
                    , El.spacing
                        40
                    ]
                    [ touchControlsView
                        viewport
                    , keyboardControlsView
                        viewport
                    ]
                )
            ]
            viewport
        ]


keyboardControlsView : Viewport -> Element msg
keyboardControlsView viewport =
    let
        container =
            El.column
                [ El.width fill
                , El.spacing
                    30
                ]

        header =
            El.el
                [ Font.color yellow
                , El.centerX
                , RFont.size
                    30
                    viewport
                ]
                (El.text "Keyboard Controls")
    in
    RUI.select
        (DefaultView
            (container
                [ header
                , viewport
                    |> keyboardControlsInTwoRows
                        keyboardControls
                ]
            )
        )
        [ PhonePortrait
            (container
                [ header
                , keyboardControls
                    |> List.unzip
                    |> keyboardControlsColumns
                ]
            )
        ]
        viewport


keyboardControlsColumns : ( List String, List String ) -> Element msg
keyboardControlsColumns ( labels, keys ) =
    El.row
        [ El.width fill
        , El.spacing
            10
        ]
        [ El.column
            [ El.width fill
            , El.spacing
                10
            ]
            (labels
                |> List.map
                    (\label ->
                        El.el
                            [ Font.color blue
                            , Font.alignRight
                            , El.width fill
                            ]
                            (El.text label)
                    )
            )
        , El.column
            [ El.width fill
            , El.spacing
                10
            ]
            (keys
                |> List.map
                    (\key ->
                        El.el
                            [ Font.color lightblue ]
                            (El.text key)
                    )
            )
        ]


keyboardControlsInOneRow : List ( String, String ) -> Viewport -> Element msg
keyboardControlsInOneRow controls viewport =
    El.column
        [ El.width fill
        , El.spacing
            10
        , RFont.size
            10
            viewport
        ]
        [ El.row
            [ El.width fill
            , El.spaceEvenly
            ]
            (controls
                |> List.map
                    (\( label, key ) ->
                        El.row
                            [ El.spacing
                                10
                            ]
                            [ El.el
                                [ Font.color blue ]
                                (El.text label)
                            , El.el
                                [ Font.color lightblue ]
                                (El.text key)
                            ]
                    )
            )
        ]


keyboardControlsInTwoRows : List ( String, String ) -> Viewport -> Element msg
keyboardControlsInTwoRows controls viewport =
    let
        top1 =
            controls
                |> List.take
                    3
                |> List.take
                    1

        top2 =
            controls
                |> List.take
                    3
                |> List.drop
                    2

        top3 =
            controls
                |> List.take
                    3
                |> List.drop
                    1
                |> List.take
                    1

        topRow =
            [ top1
            , top2
            , top3
            ]
                |> List.concat

        bottomRow =
            controls
                |> List.drop
                    3
    in
    El.column
        [ El.width fill
        , El.spacing
            10
        , RFont.size
            16
            viewport
        ]
        [ El.row
            [ El.width fill
            , El.spaceEvenly
            ]
            (topRow
                |> List.map
                    (\( label, key ) ->
                        El.row
                            [ El.spacing
                                10
                            ]
                            [ El.el
                                [ Font.color blue ]
                                (El.text label)
                            , El.el
                                [ Font.color lightblue ]
                                (El.text key)
                            ]
                    )
            )
        , El.row
            [ El.width fill
            , El.spaceEvenly
            ]
            (bottomRow
                |> List.map
                    (\( label, key ) ->
                        El.row
                            [ El.spacing
                                10
                            ]
                            [ El.el
                                [ Font.color blue ]
                                (El.text label)
                            , El.el
                                [ Font.color lightblue ]
                                (El.text key)
                            ]
                    )
            )
        ]


keyboardControls : List ( String, String )
keyboardControls =
    [ ( "Move Left:"
      , "Left Arrow"
      )
    , ( "Move Right:"
      , "Right Arrow"
      )
    , ( "Shoot:"
      , "Spacebar"
      )
    , ( "Pause:"
      , "P"
      )
    , ( "Resume:"
      , "R"
      )
    , ( "Quit:"
      , "Q"
      )
    ]


touchControlsView : Viewport -> Element msg
touchControlsView viewport =
    El.column
        [ Font.color blue
        , El.width fill
        , Rel.spacing
            30
            viewport
        ]
        [ El.el
            [ Font.color yellow
            , El.centerX
            , RFont.size
                30
                viewport
            ]
            (El.text "Touch Controls")
        , El.row
            [ El.width fill
            , Rel.spacing
                40
                viewport
            ]
            [ El.row
                [ El.width fill ]
                [ El.row
                    [ El.alignRight
                    , Rel.spacing
                        10
                        viewport
                    ]
                    [ El.el
                        []
                        (El.text "Move Left")
                    , El.el
                        [ El.height
                            (El.px
                                40
                            )
                        , El.width
                            (El.px
                                40
                            )
                        ]
                        (TouchLeft.view
                            50
                        )
                    ]
                ]
            , El.row
                [ El.width fill ]
                [ El.row
                    [ Rel.spacing
                        10
                        viewport
                    ]
                    [ El.el
                        [ El.height
                            (El.px
                                40
                            )
                        , El.width
                            (El.px
                                40
                            )
                        ]
                        (TouchRight.view
                            50
                        )
                    , El.el
                        []
                        (El.text "Move Right")
                    ]
                ]
            ]
        , El.column
            [ El.width fill
            , Rel.spacing
                10
                viewport
            ]
            [ El.el
                [ El.centerX
                , El.height
                    (El.px
                        50
                    )
                , El.width
                    (El.px
                        50
                    )
                ]
                (TouchFire.view
                    50
                )
            , El.el
                [ El.centerX ]
                (El.text "Fire")
            ]
        ]
