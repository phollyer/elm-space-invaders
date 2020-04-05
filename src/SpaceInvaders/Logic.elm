module SpaceInvaders.Logic exposing
    ( GameState(..), GameData
    , init
    , Msg, update, updateTouch
    , subscriptions
    , alienLasers, aliens, bunkers, difficulty, laser, mothership, mothershipLasers, players, ship, state
    )

{-| The game logic.

@docs GameState, GameData


# INITIALIZING

@docs init


# UPDATING

@docs Msg, update, updateTouch


# SUBSCRIBING

@docs subscriptions


# QUERYING

@docs alienLasers, aliens, bunkers, difficulty, laser, mothership, mothershipLasers, players, ship, state

-}

import Browser.Events exposing (onAnimationFrameDelta)
import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.Level as Level exposing (Difficulty, Level)
import Shared.Movement as Movement
import Shared.Players as Players exposing (GameState(..), Player, Players)
import Shared.Scores as Scores
import SpaceInvaders.Assets.Aliens as Aliens exposing (Aliens)
import SpaceInvaders.Assets.Bunkers as Bunkers exposing (Bunkers)
import SpaceInvaders.Assets.HitTest as HitTest
import SpaceInvaders.Assets.Laser as Laser exposing (Laser)
import SpaceInvaders.Assets.Lasers as Lasers exposing (Lasers)
import SpaceInvaders.Assets.Mothership as Mothership exposing (Mothership)
import SpaceInvaders.Assets.Ship as Ship exposing (Ship)
import SpaceInvaders.Cache as Cache
import SpaceInvaders.Configs as Configs
import SpaceInvaders.Controls.Controls as Controls
import SpaceInvaders.PlayerState as PlayerState
import SpaceInvaders.SFX as SFX exposing (SFX, Sound(..), Status(..))
import Time



-- MODEL


{-| A union type representing all the possible [State](#State)s of the Game.
-}
type GameState
    = IntroducingPlayer Int
    | Playing
    | MovingToNextLevel
    | AliensReachedGround
    | LifeLost
    | GameOver
    | AllGamesOver
    | Pause
    | Quit


{-| A type representing all the Game data required to play the Game.

This is an opaque type, use the exposed API to interact with it.

-}
type GameData
    = GameData
        { -- Assets
          ship : Ship
        , laser : Laser
        , bunkers : Bunkers
        , aliens : Aliens
        , alienLasers : Lasers
        , mothership : Mothership
        , mothershipLasers : Lasers

        -- Information
        , animateDelta : Float
        , difficulty : Difficulty
        , level : Level
        , players : Players PlayerState.State
        , state : GameState

        -- Sound Effects
        , sfx : SFX
        }



-- INIT


{-| All the initial settings for the game.
-}
init : Level -> Difficulty -> Players PlayerState.State -> GameData
init level_ difficulty_ players_ =
    GameData
        { aliens =
            difficulty_
                |> Aliens.init level_
        , alienLasers = Lasers.init
        , animateDelta = 0
        , bunkers = Bunkers.init
        , difficulty = difficulty_
        , laser = Laser.init "yellow" 10
        , level = level_
        , mothership = Mothership.init
        , mothershipLasers = Lasers.init
        , players = players_
        , ship = Ship.init
        , state = IntroducingPlayer 1
        , sfx = SFX.init
        }



-- UPDATE


{-| -}
type Msg
    = Animate Float
    | NewAlienLasers (List Int)
    | NextMothershipDueIn Int
    | SendNextMothership Time.Posix
    | LoadMothershipLasers (List Int)
    | ControlsMsg Controls.Msg
    | LifeLostMsg Time.Posix
    | AliensReachedGroundMsg Time.Posix
    | NextPlayerMsg Time.Posix
    | PlayerIntroduced Time.Posix
    | GameOverMsg Time.Posix


{-| -}
update : Msg -> GameData -> ( GameData, Cmd Msg )
update msg game =
    case msg of
        Animate delta ->
            let
                gamePlay =
                    game
                        |> updateAnimateDelta
                            delta
                        |> updateSfx
                            SFX.init
                        |> play
            in
            case gamePlay |> state of
                MovingToNextLevel ->
                    let
                        nextLevel =
                            gamePlay
                                |> level
                                |> Level.next
                    in
                    ( gamePlay
                        |> updateState
                            Playing
                        |> updateLevel
                            nextLevel
                        |> updateBunkers
                            Bunkers.init
                        |> updateAliens
                            (gamePlay
                                |> difficulty
                                |> Aliens.init
                                    nextLevel
                            )
                    , Cmd.none
                    )

                AliensReachedGround ->
                    ( gamePlay
                        |> updateShip
                            (gamePlay
                                |> ship
                                |> Ship.kill
                            )
                    , gamePlay
                        |> sfx
                        |> SFX.add
                            HitPlayer
                        |> SFX.add
                            (SFX.Mothership Stop)
                        |> SFX.execute
                    )

                LifeLost ->
                    ( gamePlay
                    , gamePlay
                        |> sfx
                        |> SFX.add
                            (SFX.Mothership Stop)
                        |> SFX.execute
                    )

                Playing ->
                    let
                        ( mothership_, mothershipCmd ) =
                            gamePlay
                                |> mothership
                                |> Mothership.maybeOrder
                                    NextMothershipDueIn
                                    (gamePlay
                                        |> level
                                    )
                                    (gamePlay
                                        |> aliens
                                        |> Aliens.remaining
                                    )
                    in
                    ( gamePlay
                        |> updateMothership
                            mothership_
                    , [ mothershipCmd
                      , gamePlay
                            |> aliens
                            |> Aliens.maybeNewLasers
                                NewAlienLasers
                                (gamePlay
                                    |> alienLasers
                                    |> Lasers.list
                                )
                      , gamePlay
                            |> sfx
                            |> SFX.execute
                      ]
                        |> Cmd.batch
                    )

                _ ->
                    ( gamePlay, Cmd.none )

        NextMothershipDueIn time ->
            ( game
                |> updateMothership
                    (game
                        |> mothership
                        |> Mothership.nextDueIn
                            time
                    )
            , SFX.init
                |> SFX.add
                    (Mothership Stop)
                |> SFX.execute
            )

        SendNextMothership _ ->
            ( game
                |> updateMothership
                    (game
                        |> mothership
                        |> Mothership.send
                    )
            , [ Mothership.newLasers
                    LoadMothershipLasers
                    (game
                        |> level
                    )
                    (game
                        |> difficulty
                    )
              , SFX.init
                    |> SFX.add
                        (Mothership Play)
                    |> SFX.execute
              ]
                |> Cmd.batch
            )

        LoadMothershipLasers xPoints ->
            ( game
                |> updateMothership
                    (game
                        |> mothership
                        |> Mothership.loadLasers
                            xPoints
                    )
            , Cmd.none
            )

        NewAlienLasers indices ->
            ( game
                |> updateAliens
                    (game
                        |> aliens
                        |> Aliens.loadLasers
                            indices
                    )
            , Cmd.none
            )

        AliensReachedGroundMsg _ ->
            ( game
                |> updateAliens
                    (Aliens.init
                        (game
                            |> level
                        )
                        (game
                            |> difficulty
                        )
                    )
                |> updateBunkers
                    Bunkers.init
                |> lifeLost
                |> storePlayerState
                |> nextPlayer
            , Cmd.none
            )

        LifeLostMsg _ ->
            ( game
                |> lifeLost
                |> storePlayerState
                |> nextPlayer
            , SFX.init
                |> SFX.add
                    (SFX.Mothership Stop)
                |> SFX.execute
            )

        NextPlayerMsg _ ->
            ( game
                |> nextPlayer
            , Cmd.none
            )

        PlayerIntroduced _ ->
            ( game
                |> updateState
                    Playing
            , SFX.init
                |> SFX.add
                    (game
                        |> mothership
                        |> Mothership.maybePlaySFX
                    )
                |> SFX.execute
            )

        GameOverMsg _ ->
            let
                players_ =
                    game
                        |> players
                        |> Players.gameOverForCurrentPlayer
            in
            ( game
                |> updatePlayers
                    players_
                |> updateState
                    GameOver
                |> storePlayerState
            , [ players_
                    |> Players.highscores
                    |> Scores.prepareForSave
                    |> Cache.saveHighscores
              , SFX.init
                    |> SFX.add
                        (SFX.Mothership Stop)
                    |> SFX.execute
              ]
                |> Cmd.batch
            )

        ControlsMsg ctrlMsg ->
            case ctrlMsg of
                Controls.KeyDown Controls.LeftArrow ->
                    ( game
                        |> updateShip
                            (game
                                |> ship
                                |> Ship.moveLeft
                            )
                    , Cmd.none
                    )

                Controls.KeyDown Controls.RightArrow ->
                    ( game
                        |> updateShip
                            (game
                                |> ship
                                |> Ship.moveRight
                            )
                    , Cmd.none
                    )

                Controls.KeyUp Controls.LeftArrow ->
                    ( game
                        |> updateShip
                            (game
                                |> ship
                                |> Ship.stop
                            )
                    , Cmd.none
                    )

                Controls.KeyUp Controls.RightArrow ->
                    ( game
                        |> updateShip
                            (game
                                |> ship
                                |> Ship.stop
                            )
                    , Cmd.none
                    )

                Controls.KeyDown Controls.SpaceBar ->
                    case game |> laser |> Laser.isActive of
                        True ->
                            ( game
                            , Cmd.none
                            )

                        False ->
                            ( game
                                |> updateLaser
                                    (game
                                        |> laser
                                        |> Laser.load
                                            (game
                                                |> ship
                                                |> Ship.fire
                                            )
                                        |> Laser.fire
                                    )
                            , SFX.init
                                |> SFX.add
                                    Fire
                                |> SFX.execute
                            )

                Controls.KeyDown Controls.Pause ->
                    ( game
                        |> updateState
                            Pause
                    , SFX.init
                        |> SFX.add
                            (SFX.Mothership Stop)
                        |> SFX.execute
                    )

                Controls.KeyUp Controls.Resume ->
                    ( game
                        |> updateState
                            Playing
                    , SFX.init
                        |> SFX.add
                            (game
                                |> mothership
                                |> Mothership.maybePlaySFX
                            )
                        |> SFX.execute
                    )

                Controls.KeyUp Controls.Quit ->
                    ( game
                        |> updateState
                            Quit
                    , SFX.init
                        |> SFX.add
                            (SFX.Mothership Stop)
                        |> SFX.execute
                    )

                _ ->
                    ( game, Cmd.none )


{-| -}
updateTouch : Controls.Touch -> GameData -> ( GameData, Cmd Msg )
updateTouch touchMsg game =
    case touchMsg of
        Controls.TouchLeft _ ->
            ( game
                |> updateShip
                    (game
                        |> ship
                        |> Ship.moveLeft
                    )
            , Cmd.none
            )

        Controls.TouchRight _ ->
            ( game
                |> updateShip
                    (game
                        |> ship
                        |> Ship.moveRight
                    )
            , Cmd.none
            )

        Controls.TouchStop _ ->
            ( game
                |> updateShip
                    (game
                        |> ship
                        |> Ship.stop
                    )
            , Cmd.none
            )

        Controls.TouchFire _ ->
            case game |> laser |> Laser.isActive of
                True ->
                    ( game
                    , Cmd.none
                    )

                False ->
                    ( game
                        |> updateLaser
                            (game
                                |> laser
                                |> Laser.load
                                    (game
                                        |> ship
                                        |> Ship.fire
                                    )
                                |> Laser.fire
                            )
                    , SFX.init
                        |> SFX.add
                            Fire
                        |> SFX.execute
                    )

        Controls.TouchPause _ ->
            ( game
                |> updateState
                    Pause
            , SFX.init
                |> SFX.add
                    (SFX.Mothership Stop)
                |> SFX.execute
            )

        Controls.TouchResume _ ->
            ( game
                |> updateState
                    Playing
            , SFX.init
                |> SFX.add
                    (game
                        |> mothership
                        |> Mothership.maybePlaySFX
                    )
                |> SFX.execute
            )

        Controls.TouchQuit _ ->
            ( game
                |> updateState
                    Quit
            , SFX.init
                |> SFX.add
                    (SFX.Mothership Stop)
                |> SFX.execute
            )



-- SUBSCRIPTIONS


{-| -}
subscriptions : GameData -> Sub Msg
subscriptions game =
    let
        controls =
            Controls.subscriptions
                |> Sub.map
                    ControlsMsg
    in
    case game |> state of
        Playing ->
            Sub.batch
                [ controls
                , onAnimationFrameDelta Animate
                , game
                    |> mothership
                    |> Mothership.subscriptions
                        SendNextMothership
                ]

        AliensReachedGround ->
            case game |> players |> Players.current |> Players.lastLife of
                True ->
                    Sub.batch
                        [ controls
                        , GameOverMsg
                            |> Time.every 10
                        ]

                False ->
                    Sub.batch
                        [ controls
                        , AliensReachedGroundMsg
                            |> Time.every 1500
                        ]

        LifeLost ->
            case game |> players |> Players.current |> Players.lastLife of
                True ->
                    Sub.batch
                        [ controls
                        , GameOverMsg
                            |> Time.every 10
                        ]

                False ->
                    Sub.batch
                        [ controls
                        , LifeLostMsg
                            |> Time.every 1500
                        ]

        IntroducingPlayer _ ->
            Sub.batch
                [ controls
                , PlayerIntroduced
                    |> Time.every 2500
                ]

        GameOver ->
            Sub.batch
                [ controls
                , NextPlayerMsg
                    |> Time.every 3000
                ]

        _ ->
            controls



-- INTERNAL GAME LOGIC


play : GameData -> GameData
play game =
    game
        |> moveAssets
        |> detectCollisions
        |> fireLasers
        |> setState



--  MOVE ASSETS


moveAssets : GameData -> GameData
moveAssets game =
    game
        |> moveShip
        |> moveLaser
        |> moveAliens
        |> moveAlienLasers
        |> moveMothership
        |> moveMothershipLasers


moveShip : GameData -> GameData
moveShip game =
    game
        |> updateShip
            (game
                |> ship
                |> Ship.move
            )


moveLaser : GameData -> GameData
moveLaser game =
    let
        ( laser_, didMove ) =
            game
                |> laser
                |> Laser.maybeMove
                    (Movement.Up Configs.laser.velocity)
    in
    case didMove of
        True ->
            game
                |> updateLaser
                    laser_

        False ->
            game


moveAliens : GameData -> GameData
moveAliens game =
    let
        aliens_ =
            game
                |> aliens
    in
    game
        |> updateAliens
            (aliens_
                |> Aliens.maybeMove
                    (game
                        |> animateDelta
                    )
            )
        |> updateSfx
            (case aliens_ |> Aliens.playSFX of
                True ->
                    game
                        |> sfx
                        |> SFX.add
                            (aliens_
                                |> Aliens.sfx
                            )

                False ->
                    game
                        |> sfx
            )


moveAlienLasers : GameData -> GameData
moveAlienLasers game =
    game
        |> updateAlienLasers
            (game
                |> alienLasers
                |> Lasers.maybeMove
                    (Movement.Down Configs.alienLaser.velocity)
            )


moveMothership : GameData -> GameData
moveMothership game =
    game
        |> updateMothership
            (game
                |> mothership
                |> Mothership.maybeMove
            )


moveMothershipLasers : GameData -> GameData
moveMothershipLasers game =
    game
        |> updateMothershipLasers
            (game
                |> mothershipLasers
                |> Lasers.maybeMove
                    (Movement.Down Configs.mothership.laserSpeed)
            )



-- DETECT COLLISIONS


detectCollisions : GameData -> GameData
detectCollisions game =
    game
        |> detectPlayerHitAlien
        |> detectPlayerHitByAlienLaser
        |> detectPlayerHitByAlien
        |> detectPlayerHitMothership
        |> detectPlayerHitByMothershipLaser
        |> detectPlayerHitBunker
        |> detectBunkersHitByAlienLasers
        |> detectBunkersHitByMothershipLasers
        |> detectBunkersHitByAliens



-- DETECT PLAYER LASER HITS


detectPlayerHitAlien : GameData -> GameData
detectPlayerHitAlien game =
    let
        ( aliens_, hit, points ) =
            game
                |> laser
                |> HitTest.detectPlayerHitAlien
                    (game
                        |> aliens
                    )
    in
    case hit of
        True ->
            game
                |> updateAliens
                    (aliens_
                        |> Aliens.maybeChangeSpeed
                    )
                |> updateLaser
                    (game
                        |> laser
                        |> Laser.didHit
                    )
                |> updatePlayers
                    (game
                        |> players
                        |> Players.incrementScoreForCurrentPlayer
                            points
                    )
                |> updateSfx
                    (game
                        |> sfx
                        |> SFX.add
                            HitAlien
                    )

        False ->
            game


detectPlayerHitMothership : GameData -> GameData
detectPlayerHitMothership game =
    let
        ( hit, points ) =
            game
                |> laser
                |> HitTest.detectPlayerHitMothership
                    (game
                        |> mothership
                    )
    in
    case hit of
        True ->
            game
                |> updateMothership
                    Mothership.init
                |> updateLaser
                    (game
                        |> laser
                        |> Laser.didHit
                    )
                |> updatePlayers
                    (game
                        |> players
                        |> Players.incrementScoreForCurrentPlayer
                            points
                    )
                |> updateSfx
                    (game
                        |> sfx
                        |> SFX.add
                            HitMothership
                        |> SFX.add
                            (Mothership Stop)
                    )

        False ->
            game


detectPlayerHitBunker : GameData -> GameData
detectPlayerHitBunker game =
    let
        ( bunkers_, hit ) =
            game
                |> bunkers
                |> HitTest.detectPlayerHitBunker
                    (game
                        |> laser
                    )
    in
    case hit of
        True ->
            game
                |> updateBunkers bunkers_
                |> updateLaser
                    (game
                        |> laser
                        |> Laser.didHit
                    )
                |> updateSfx
                    (game
                        |> sfx
                        |> SFX.add
                            HitBunker
                    )

        False ->
            game


detectPlayerHitByMothershipLaser : GameData -> GameData
detectPlayerHitByMothershipLaser game =
    let
        ( lasers, hit ) =
            game
                |> ship
                |> HitTest.detectPlayerHitByMothershipLaser
                    (game
                        |> mothershipLasers
                    )
    in
    case hit of
        True ->
            game
                |> updateMothershipLasers lasers
                |> updateShip
                    (game
                        |> ship
                        |> Ship.kill
                    )
                |> updateSfx
                    (game
                        |> sfx
                        |> SFX.add
                            HitPlayer
                    )

        False ->
            game


detectPlayerHitByAlienLaser : GameData -> GameData
detectPlayerHitByAlienLaser game =
    let
        ( lasers, hit ) =
            game
                |> ship
                |> HitTest.detectPlayerHitByAlienLaser
                    (game
                        |> alienLasers
                    )
    in
    case hit of
        True ->
            game
                |> updateAlienLasers lasers
                |> updateShip
                    (game
                        |> ship
                        |> Ship.kill
                    )
                |> updateSfx
                    (game
                        |> sfx
                        |> SFX.add
                            HitPlayer
                    )

        False ->
            game


detectPlayerHitByAlien : GameData -> GameData
detectPlayerHitByAlien game =
    let
        ( aliens_, hit ) =
            game
                |> ship
                |> HitTest.detectPlayerHitByAlien
                    (game
                        |> aliens
                    )
    in
    case hit of
        True ->
            game
                |> updateAliens
                    (aliens_
                        |> Aliens.maybeChangeSpeed
                    )
                |> updateShip
                    (game
                        |> ship
                        |> Ship.kill
                    )
                |> updateSfx
                    (game
                        |> sfx
                        |> SFX.add
                            HitPlayer
                    )

        False ->
            game


detectBunkersHitByAliens : GameData -> GameData
detectBunkersHitByAliens game =
    let
        ( bunkers_, hit ) =
            game
                |> bunkers
                |> HitTest.detectBunkersHitByAliens
                    (game
                        |> aliens
                    )
    in
    case hit of
        True ->
            game
                |> updateBunkers
                    bunkers_
                |> updateSfx
                    (game
                        |> sfx
                        |> SFX.add
                            HitBunker
                    )

        False ->
            game


detectBunkersHitByAlienLasers : GameData -> GameData
detectBunkersHitByAlienLasers game =
    let
        ( alienLasers_, bunkers_, hit ) =
            game
                |> bunkers
                |> HitTest.detectBunkersHitByAlienLasers
                    (game
                        |> alienLasers
                    )
    in
    case hit of
        True ->
            game
                |> updateAlienLasers
                    alienLasers_
                |> updateBunkers
                    bunkers_
                |> updateSfx
                    (game
                        |> sfx
                        |> SFX.add
                            HitBunker
                    )

        False ->
            game


detectBunkersHitByMothershipLasers : GameData -> GameData
detectBunkersHitByMothershipLasers game =
    let
        ( mothershipLasers_, bunkers_, hit ) =
            game
                |> bunkers
                |> HitTest.detectBunkersHitByMothershipLasers
                    (game
                        |> mothershipLasers
                    )
    in
    case hit of
        True ->
            game
                |> updateMothershipLasers
                    mothershipLasers_
                |> updateBunkers
                    bunkers_
                |> updateSfx
                    (game
                        |> sfx
                        |> SFX.add
                            HitBunker
                    )

        False ->
            game



-- FIRE ALIEN LASERS


fireLasers : GameData -> GameData
fireLasers game =
    case game |> state of
        Playing ->
            game
                |> fireAlienLasers
                |> fireMothershipLasers

        _ ->
            game


fireAlienLasers : GameData -> GameData
fireAlienLasers game =
    let
        ( firingPoints, aliens_ ) =
            game
                |> aliens
                |> Aliens.maybeFire
    in
    case firingPoints of
        _ :: _ ->
            game
                |> updateAliens
                    aliens_
                |> updateAlienLasers
                    (game
                        |> alienLasers
                        |> Lasers.add
                            (firingPoints
                                |> Laser.loadAll
                                    "salmon"
                                    Configs.alienLaser.height
                                |> Laser.fireAll
                            )
                    )

        [] ->
            game


fireMothershipLasers : GameData -> GameData
fireMothershipLasers game =
    let
        ( maybePoint, mothership_ ) =
            game
                |> mothership
                |> Mothership.maybeFire
    in
    case maybePoint of
        Just point ->
            game
                |> updateMothership mothership_
                |> updateMothershipLasers
                    (game
                        |> mothershipLasers
                        |> Lasers.add
                            (point
                                |> Laser.fromPoint
                                    "blue"
                                    10
                                |> Laser.fire
                                |> List.singleton
                            )
                    )

        Nothing ->
            game


setState : GameData -> GameData
setState game =
    let
        newState =
            case game |> players |> Players.anyAlive of
                False ->
                    AllGamesOver

                True ->
                    case game |> players |> Players.current |> Players.anyLivesLeft of
                        False ->
                            GameOver

                        True ->
                            case game |> ship |> Ship.alive of
                                False ->
                                    LifeLost

                                True ->
                                    case game |> aliens |> Aliens.anyLeft of
                                        False ->
                                            MovingToNextLevel

                                        True ->
                                            case game |> aliens |> Aliens.aboveGround of
                                                False ->
                                                    AliensReachedGround

                                                True ->
                                                    game
                                                        |> state
    in
    game
        |> updateState
            newState


nextPlayer : GameData -> GameData
nextPlayer game =
    case game |> players |> Players.anyAlive of
        True ->
            game
                |> gotoNextPlayer

        False ->
            game
                |> updateState
                    AllGamesOver


gotoNextPlayer : GameData -> GameData
gotoNextPlayer game =
    let
        players_ =
            game
                |> players
                |> Players.next

        currentPlayer =
            players_
                |> Players.current

        number =
            currentPlayer
                |> Players.number
    in
    game
        |> restoreState
            currentPlayer
        |> updatePlayers
            players_
        |> updateState
            (IntroducingPlayer number)
        |> updateShip
            Ship.init


restoreState : Player PlayerState.State -> GameData -> GameData
restoreState player (GameData gameData) =
    let
        playerState =
            case player |> Players.state of
                State state_ ->
                    state_

                NoState ->
                    GameData gameData
                        |> difficulty
                        |> PlayerState.init
    in
    gameData
        |> PlayerState.restore
            playerState
        |> GameData


lifeLost : GameData -> GameData
lifeLost game =
    game
        |> updatePlayers
            (game
                |> players
                |> Players.killCurrentPlayer
            )


storePlayerState : GameData -> GameData
storePlayerState (GameData gameData) =
    GameData gameData
        |> updatePlayers
            (GameData gameData
                |> players
                |> Players.storeStateForCurrentPlayer
                    (gameData
                        |> PlayerState.store
                    )
            )



-- QUERYING


{-| -}
aliens : GameData -> Aliens
aliens (GameData gameData) =
    gameData.aliens


{-| -}
alienLasers : GameData -> Lasers
alienLasers (GameData gameData) =
    gameData.alienLasers


animateDelta : GameData -> Float
animateDelta (GameData gameData) =
    gameData.animateDelta


{-| -}
bunkers : GameData -> Bunkers
bunkers (GameData gameData) =
    gameData.bunkers


{-| -}
difficulty : GameData -> Difficulty
difficulty (GameData gameData) =
    gameData.difficulty


{-| -}
laser : GameData -> Laser
laser (GameData gameData) =
    gameData.laser


level : GameData -> Level
level (GameData gameData) =
    gameData.level


{-| -}
mothership : GameData -> Mothership
mothership (GameData gameData) =
    gameData.mothership


{-| -}
mothershipLasers : GameData -> Lasers
mothershipLasers (GameData gameData) =
    gameData.mothershipLasers


{-| -}
players : GameData -> Players PlayerState.State
players (GameData gameData) =
    gameData.players


{-| -}
ship : GameData -> Ship
ship (GameData gameData) =
    gameData.ship


sfx : GameData -> SFX
sfx (GameData gameData) =
    gameData.sfx


{-| -}
state : GameData -> GameState
state (GameData gameData) =
    gameData.state



-- UPDATING


updateAliens : Aliens -> GameData -> GameData
updateAliens aliens_ (GameData gameData) =
    GameData
        { gameData
            | aliens = aliens_
        }


updateAlienLasers : Lasers -> GameData -> GameData
updateAlienLasers alienLasers_ (GameData gameData) =
    GameData
        { gameData
            | alienLasers = alienLasers_
        }


updateAnimateDelta : Float -> GameData -> GameData
updateAnimateDelta animateDelta_ (GameData gameData) =
    GameData
        { gameData
            | animateDelta = animateDelta_
        }


updateBunkers : Bunkers -> GameData -> GameData
updateBunkers bunkers_ (GameData gameData) =
    GameData
        { gameData
            | bunkers = bunkers_
        }


updateLaser : Laser -> GameData -> GameData
updateLaser laser_ (GameData gameData) =
    GameData
        { gameData
            | laser = laser_
        }


updateLevel : Level -> GameData -> GameData
updateLevel level_ (GameData gameData) =
    GameData
        { gameData
            | level = level_
        }


updateMothership : Mothership -> GameData -> GameData
updateMothership mothership_ (GameData gameData) =
    GameData
        { gameData
            | mothership = mothership_
        }


updateMothershipLasers : Lasers -> GameData -> GameData
updateMothershipLasers mothershipLasers_ (GameData gameData) =
    GameData
        { gameData
            | mothershipLasers = mothershipLasers_
        }


updatePlayers : Players PlayerState.State -> GameData -> GameData
updatePlayers players_ (GameData gameData) =
    GameData
        { gameData
            | players = players_
        }


updateSfx : SFX -> GameData -> GameData
updateSfx sfx_ (GameData gameData) =
    GameData
        { gameData
            | sfx = sfx_
        }


updateShip : Ship -> GameData -> GameData
updateShip ship_ (GameData gameData) =
    GameData
        { gameData
            | ship = ship_
        }


updateState : GameState -> GameData -> GameData
updateState state_ (GameData gameData) =
    GameData
        { gameData
            | state = state_
        }
