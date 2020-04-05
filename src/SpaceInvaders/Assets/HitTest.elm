module SpaceInvaders.Assets.HitTest exposing
    ( detectPlayerHitAlien, detectPlayerHitMothership, detectPlayerHitBunker
    , detectPlayerHitByAlien, detectPlayerHitByAlienLaser, detectPlayerHitByMothershipLaser
    , detectBunkersHitByAlienLasers, detectBunkersHitByMothershipLasers, detectBunkersHitByAliens
    )

{-| Check for [Hit](Shared.HitTest#Hit)s between game assets.


# PLAYER HITS

@docs detectPlayerHitAlien, detectPlayerHitMothership, detectPlayerHitBunker


# PLAYER HIT BY

@docs detectPlayerHitByAlien, detectPlayerHitByAlienLaser, detectPlayerHitByMothershipLaser


# BUNKER HITS

@docs detectBunkersHitByAlienLasers, detectBunkersHitByMothershipLasers, detectBunkersHitByAliens

-}

import Shared.BoundingBox as BoundingBox exposing (BoundingBox)
import Shared.HitTest as HitTest exposing (Hit(..), Primary(..), Target(..))
import Shared.Point exposing (Point)
import SpaceInvaders.Assets.Aliens as Aliens exposing (Aliens)
import SpaceInvaders.Assets.Bunkers as Bunkers exposing (Bunker, Bunkers)
import SpaceInvaders.Assets.Laser as Laser exposing (Laser)
import SpaceInvaders.Assets.Lasers as Lasers exposing (Lasers)
import SpaceInvaders.Assets.Mothership as Mothership exposing (Mothership)
import SpaceInvaders.Assets.Ship as Ship exposing (Ship)



-- PLAYER HITS


{-| Detect if a player's laser hits an
[Alien](SpaceInvaders.Assets.Aliens#Alien).

Returns a three
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
as follows:

    ( Aliens
      -- updated to reflect any hits.
    , Bool
      -- whether there was a hit.
    , Int
      -- the points scored.
    )

-}
detectPlayerHitAlien : Aliens -> Laser -> ( Aliens, Bool, Int )
detectPlayerHitAlien aliens laser =
    case laser |> Laser.isActive of
        True ->
            let
                pruner =
                    aliens
                        |> Aliens.rows
                        |> detectHit
                            (laser
                                |> Laser.boundingBox
                            )
            in
            case pruner.hit of
                Miss ->
                    ( aliens
                    , False
                    , 0
                    )

                Resting _ ->
                    ( aliens
                    , False
                    , 0
                    )

                _ ->
                    ( aliens
                        |> Aliens.updateRows
                            pruner.targetRows
                    , True
                    , case pruner.targetHit of
                        Just asset ->
                            asset.score

                        Nothing ->
                            0
                    )

        False ->
            ( aliens
            , False
            , 0
            )


{-| Detect if a player's laser hits a
[Mothership](SpaceInvaders.Assets.Mothership#Mothership).

Returns a two
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
as follows:

    ( Bool
      -- whether there was a hit.
    , Int
      -- the points scored.
    )

-}
detectPlayerHitMothership : Mothership -> Laser -> ( Bool, Int )
detectPlayerHitMothership mothership laser =
    case (laser |> Laser.isActive) && Mothership.isMoving mothership of
        True ->
            case Primary (Laser.boundingBox laser) |> HitTest.detect (mothership |> Mothership.boundingBox |> Target) of
                Miss ->
                    ( False
                    , 0
                    )

                Resting _ ->
                    ( False
                    , 0
                    )

                _ ->
                    ( True
                    , 50
                    )

        False ->
            ( False
            , 0
            )


{-| Detect if a player's laser hits a
[Bunker](SpaceInvaders.Assets.Bunkers#Bunker).

Returns a two
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
as follows:

    ( Bunkers
      -- updated to reflect any hits.
    , Bool
      -- whether there was a hit.
    )

-}
detectPlayerHitBunker : Laser -> Bunkers -> ( Bunkers, Bool )
detectPlayerHitBunker laser bunkers =
    case laser |> Laser.isActive of
        True ->
            let
                ( bunkers_, hit, _ ) =
                    ( bunkers, False, laser |> Laser.boundingBox )
                        |> detectLeftBunkerHit
                        |> detectMiddleBunkerHit
                        |> detectRightBunkerHit
            in
            ( bunkers_
            , hit
            )

        False ->
            ( bunkers
            , False
            )



-- PLAYER HIT BY


{-| Detect if a player's [Ship](SpaceInvaders.Assets.Ship#Ship) gets hit by an
[Alien](SpaceInvaders.Assets.Aliens#Alien).

Returns a two
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
as follows:

    ( Aliens
      -- updated to reflect a hit.
    , Bool
      -- whether there was a hit.
    )

-}
detectPlayerHitByAlien : Aliens -> Ship -> ( Aliens, Bool )
detectPlayerHitByAlien aliens ship =
    case ship |> Ship.alive of
        True ->
            let
                pruner =
                    aliens
                        |> Aliens.rows
                        |> detectHit
                            (ship
                                |> Ship.boundingBox
                            )
            in
            case pruner.hit of
                Miss ->
                    ( aliens
                    , False
                    )

                Resting _ ->
                    ( aliens
                    , False
                    )

                _ ->
                    ( aliens
                        |> Aliens.updateRows
                            pruner.targetRows
                    , True
                    )

        False ->
            ( aliens
            , False
            )


{-| Detect if a player's [Ship](SpaceInvaders.Assets.Ship#Ship) gets hit by an
[Alien](SpaceInvaders.Assets.Aliens#Alien)
[Laser](SpaceInvaders.Assets.Laser#Laser).

Returns a two
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
as follows:

    ( Lasers
      -- updated to reflect a hit.
    , Bool
      -- whether there was a hit.
    )

-}
detectPlayerHitByAlienLaser : Lasers -> Ship -> ( Lasers, Bool )
detectPlayerHitByAlienLaser lasers ship =
    ship
        |> detectPlayerHitByLaser
            lasers


{-| Detect if a player's [Ship](SpaceInvaders.Assets.Ship#Ship) gets hit by a
[Mothership](SpaceInvaders.Assets.Mothership#Mothership)
[Laser](SpaceInvaders.Assets.Laser#Laser).

Returns a two
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
as follows:

    ( Lasers
      -- updated to reflect a hit.
    , Bool
      -- whether there was a hit.
    )

-}
detectPlayerHitByMothershipLaser : Lasers -> Ship -> ( Lasers, Bool )
detectPlayerHitByMothershipLaser lasers ship =
    ship
        |> detectPlayerHitByLaser
            lasers


detectPlayerHitByLaser : Lasers -> Ship -> ( Lasers, Bool )
detectPlayerHitByLaser lasers ship =
    case (ship |> Ship.alive) && (lasers |> Lasers.list |> List.length) > 0 of
        True ->
            case (lasers |> Lasers.boundingBox |> BoundingBox.bottom) > (ship |> Ship.boundingBox |> BoundingBox.top) of
                True ->
                    ship
                        |> testPlayerHitByLaser
                            lasers

                False ->
                    ( lasers
                    , False
                    )

        False ->
            ( lasers
            , False
            )


testPlayerHitByLaser : Lasers -> Ship -> ( Lasers, Bool )
testPlayerHitByLaser lasers ship =
    let
        newList =
            lasers
                |> Lasers.list
                |> List.filter
                    (\laser ->
                        laser
                            |> Laser.boundingBox
                            |> Primary
                            |> HitTest.detect
                                (ship
                                    |> Ship.boundingBox
                                    |> Target
                                )
                            |> (==) Miss
                    )

        hit =
            (newList |> List.length) < (lasers |> Lasers.list |> List.length)
    in
    case hit of
        True ->
            ( lasers
                |> Lasers.updateList
                    newList
                |> Lasers.calculateBoundingBox
            , True
            )

        False ->
            ( lasers
            , False
            )



-- BUNKER HITS


{-| Detect if a [Bunker](SpaceInvaders.Assets.Bunkers#Bunker) gets hit by an
[Alien](SpaceInvaders.Assets.Aliens#Alien)
[Laser](SpaceInvaders.Assets.Laser#Laser).

Returns a three
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
as follows:

    ( Lasers
      -- updated to reflect any hits.
    , Bunkers
      -- updated to reflect any hits.
    , Bool
      -- whether there was a hit.
    )

-}
detectBunkersHitByAlienLasers : Lasers -> Bunkers -> ( Lasers, Bunkers, Bool )
detectBunkersHitByAlienLasers lasers bunkers =
    bunkers
        |> detectBunkersHitByLasers
            lasers


{-| Detect if a [Bunker](SpaceInvaders.Assets.Bunkers#Bunker) gets hit by a
[Mothership](SpaceInvaders.Assets.Mothership#Mothership)
[Laser](SpaceInvaders.Assets.Laser#Laser).

Returns a three
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
as follows:

    ( Lasers
      -- updated to reflect any hits.
    , Bunkers
      -- updated to reflect any hits.
    , Bool
      -- whether there was a hit.
    )

-}
detectBunkersHitByMothershipLasers : Lasers -> Bunkers -> ( Lasers, Bunkers, Bool )
detectBunkersHitByMothershipLasers lasers bunkers =
    bunkers
        |> detectBunkersHitByLasers
            lasers


detectBunkersHitByLasers : Lasers -> Bunkers -> ( Lasers, Bunkers, Bool )
detectBunkersHitByLasers lasers bunkers =
    case (lasers |> Lasers.list |> List.length) > 0 of
        True ->
            case Primary (Lasers.boundingBox lasers) |> HitTest.detect (bunkers |> Bunkers.boundingBox |> Target) of
                Miss ->
                    ( lasers
                    , bunkers
                    , False
                    )

                Resting _ ->
                    ( lasers
                    , bunkers
                    , False
                    )

                _ ->
                    lasers
                        |> Lasers.detectBunkerHits
                            (bunkers
                                |> detectBunkersHitByAll
                            )

        False ->
            ( lasers
            , bunkers
            , False
            )


{-| Detect if a [Bunker](SpaceInvaders.Assets.Bunkers#Bunker) gets hit by an
[Alien](SpaceInvaders.Assets.Aliens#Alien).

Returns a two
[Tuple](https://package.elm-lang.org/packages/elm/core/latest/Tuple)
as follows:

    ( Bunkers
      -- updated to reflect any hits.
    , Bool
      -- whether there was a hit.
    )

-}
detectBunkersHitByAliens : Aliens -> Bunkers -> ( Bunkers, Bool )
detectBunkersHitByAliens aliens bunkers =
    case Primary (Aliens.boundingBox aliens) |> HitTest.detect (bunkers |> Bunkers.boundingBox |> Target) of
        Miss ->
            ( bunkers
            , False
            )

        Resting _ ->
            ( bunkers
            , False
            )

        _ ->
            let
                ( _, bunkers_, hit ) =
                    aliens
                        |> Aliens.list
                        |> detectBunkersHitByAll
                            bunkers
            in
            ( bunkers_
            , hit
            )


detectBunkersHitByAll : Bunkers -> List { b | boundingBox : BoundingBox } -> ( List { b | boundingBox : BoundingBox }, Bunkers, Bool )
detectBunkersHitByAll bunkers list =
    let
        results =
            list
                |> List.foldl
                    (\primary acc ->
                        let
                            ( bunkers_, hit ) =
                                acc.bunkers
                                    |> detectBunkersHitBy
                                        (primary
                                            |> .boundingBox
                                        )
                        in
                        case hit of
                            False ->
                                { acc
                                    | primaries =
                                        primary :: acc.primaries
                                    , bunkers =
                                        bunkers_
                                }

                            True ->
                                { acc
                                    | bunkers =
                                        bunkers_
                                    , bunkerHits =
                                        hit :: acc.bunkerHits
                                }
                    )
                    { primaries = []
                    , bunkers = bunkers
                    , bunkerHits = []
                    }
    in
    ( results.primaries
    , results.bunkers
    , case results.bunkerHits of
        [] ->
            False

        _ ->
            True
    )


detectBunkersHitBy : BoundingBox -> Bunkers -> ( Bunkers, Bool )
detectBunkersHitBy primary bunkers =
    let
        ( bunkers_, hit, _ ) =
            ( bunkers, False, primary )
                |> detectLeftBunkerHit
                |> detectMiddleBunkerHit
                |> detectRightBunkerHit
    in
    ( bunkers_
    , hit
    )


detectLeftBunkerHit : ( Bunkers, Bool, BoundingBox ) -> ( Bunkers, Bool, BoundingBox )
detectLeftBunkerHit ( bunkers, hit, primary ) =
    case hit of
        True ->
            ( bunkers, True, primary )

        False ->
            testBunkerHit
                ( ( bunkers
                  , Bunkers.left
                  , Bunkers.updateLeft
                  )
                , False
                , primary
                )


detectMiddleBunkerHit : ( Bunkers, Bool, BoundingBox ) -> ( Bunkers, Bool, BoundingBox )
detectMiddleBunkerHit ( bunkers, hit, primary ) =
    case hit of
        True ->
            ( bunkers, True, primary )

        False ->
            testBunkerHit
                ( ( bunkers
                  , Bunkers.middle
                  , Bunkers.updateMiddle
                  )
                , False
                , primary
                )


detectRightBunkerHit : ( Bunkers, Bool, BoundingBox ) -> ( Bunkers, Bool, BoundingBox )
detectRightBunkerHit ( bunkers, hit, primary ) =
    case hit of
        True ->
            ( bunkers, True, primary )

        False ->
            testBunkerHit
                ( ( bunkers
                  , Bunkers.right
                  , Bunkers.updateRight
                  )
                , False
                , primary
                )


testBunkerHit : ( ( Bunkers, Bunkers -> Bunker, Rows { color : String, point : Point } -> Bunkers -> Bunkers ), Bool, BoundingBox ) -> ( Bunkers, Bool, BoundingBox )
testBunkerHit ( ( bunkers, queryFunc, updateFunc ), hit, primary ) =
    case hit of
        True ->
            ( bunkers, True, primary )

        False ->
            let
                pruner =
                    bunkers
                        |> queryFunc
                        |> .rows
                        |> detectAllHits
                            primary
            in
            case pruner.targetHits of
                [] ->
                    ( bunkers, False, primary )

                _ ->
                    ( bunkers
                        |> updateFunc
                            pruner.targetRows
                    , True
                    , primary
                    )



-- PRUNER


type alias Remaining =
    Int


type alias Rows b =
    ( ( Remaining, BoundingBox ), List (Row b) )


type alias Row b =
    ( ( Remaining, BoundingBox ), List { b | boundingBox : BoundingBox } )


boundingBox : Rows b -> BoundingBox
boundingBox rows_ =
    rows_
        |> Tuple.first
        |> Tuple.second


remaining : Rows b -> Remaining
remaining rows_ =
    rows_
        |> Tuple.first
        |> Tuple.first


rows : Rows b -> List (Row b)
rows rows_ =
    rows_
        |> Tuple.second


targets : Row b -> List { b | boundingBox : BoundingBox }
targets row =
    row
        |> Tuple.second


rowBoundingBox : Row b -> BoundingBox
rowBoundingBox row =
    row
        |> Tuple.first
        |> Tuple.second


rowRemaining : Row b -> Remaining
rowRemaining row =
    row
        |> Tuple.first
        |> Tuple.first


type alias Pruner b =
    { hit : Hit
    , primary : BoundingBox
    , targetRows : Rows b
    , targetRowsHit : List (Row b)
    , targetRowsMissed : List (Row b)
    , targetRowHit : Maybe (Row b)
    , targetRowHitRemaining : Maybe (Row b)
    , targetHit : Maybe { b | boundingBox : BoundingBox }
    , targetHits : List { b | boundingBox : BoundingBox }
    }


detectAllHits : BoundingBox -> Rows b -> Pruner b
detectAllHits primary targetRows =
    primary
        |> prune
            targetRows
        |> maybePruneTargetRows
        |> maybePruneTargetRowsHit
        |> maybeRebuild


detectHit : BoundingBox -> Rows b -> Pruner b
detectHit primary targetRows =
    primary
        |> prune
            targetRows
        |> maybePruneTargetRows
        |> maybePruneTargetRowHit
        |> maybeRebuild


prune : Rows b -> BoundingBox -> Pruner b
prune targetRows primary =
    { primary = primary
    , targetRows = targetRows
    , targetRowsHit = []
    , targetRowsMissed = []
    , targetRowHit = Nothing
    , targetRowHitRemaining = Nothing
    , targetHit = Nothing
    , targetHits = []
    , hit =
        primary
            |> Primary
            |> HitTest.detect
                (targetRows
                    |> boundingBox
                    |> Target
                )
    }


maybePruneTargetRows : Pruner b -> Pruner b
maybePruneTargetRows pruner =
    case pruner.hit of
        Miss ->
            pruner

        _ ->
            pruner
                |> pruneTargetRows


pruneTargetRows : Pruner b -> Pruner b
pruneTargetRows pruner =
    let
        ( targetRowsHit, targetRowsMissed ) =
            pruner
                |> .targetRows
                |> rows
                |> List.partition
                    (\row ->
                        pruner
                            |> .primary
                            |> hitTest
                                (row
                                    |> rowBoundingBox
                                )
                    )
    in
    case targetRowsHit of
        [] ->
            { pruner
                | hit = Miss
            }

        first :: _ ->
            { pruner
                | targetRowHit = Just first
                , targetRowsHit = targetRowsHit
                , targetRowsMissed = targetRowsMissed
            }


maybePruneTargetRowsHit : Pruner b -> Pruner b
maybePruneTargetRowsHit pruner =
    case pruner.hit of
        Miss ->
            pruner

        _ ->
            pruner
                |> pruneTargetRowsHit


pruneTargetRowsHit : Pruner b -> Pruner b
pruneTargetRowsHit pruner =
    let
        ( targetHits, targetRowsMissed ) =
            pruner
                |> .targetRowsHit
                |> List.map
                    (\targetRow ->
                        let
                            ( targetHit, remaining_ ) =
                                targetRow
                                    |> targets
                                    |> List.partition
                                        (\target ->
                                            pruner
                                                |> .primary
                                                |> hitTest
                                                    (target
                                                        |> .boundingBox
                                                    )
                                        )
                        in
                        ( targetHit
                        , ( ( remaining_
                                |> List.length
                            , remaining_
                                |> List.map
                                    .boundingBox
                                |> BoundingBox.fromList
                            )
                          , remaining_
                          )
                        )
                    )
                |> List.unzip
    in
    { pruner
        | targetHits =
            targetHits
                |> List.concat
        , targetRowsMissed =
            pruner
                |> .targetRowsMissed
                |> List.append
                    targetRowsMissed
    }


maybePruneTargetRowHit : Pruner b -> Pruner b
maybePruneTargetRowHit pruner =
    case pruner.hit of
        Miss ->
            pruner

        _ ->
            case pruner.targetRowHit of
                Nothing ->
                    pruner

                Just targetRowHit ->
                    pruner
                        |> pruneTargetRowHit
                            targetRowHit


pruneTargetRowHit : Row b -> Pruner b -> Pruner b
pruneTargetRowHit targetRowHit pruner =
    let
        ( hits, remaining_ ) =
            targetRowHit
                |> targets
                |> List.partition
                    (\target ->
                        pruner
                            |> .primary
                            |> hitTest
                                (target
                                    |> .boundingBox
                                )
                    )
    in
    case hits of
        [] ->
            { pruner
                | hit = Miss
            }

        first :: _ ->
            { pruner
                | targetHit = Just first
                , targetRowHitRemaining =
                    remaining_
                        |> maybeBuildRow
            }


maybeBuildRow : List { b | boundingBox : BoundingBox } -> Maybe (Row b)
maybeBuildRow remaining_ =
    case remaining_ of
        [] ->
            Nothing

        _ ->
            Just
                ( ( remaining_
                        |> List.length
                  , remaining_
                        |> List.map
                            .boundingBox
                        |> BoundingBox.fromList
                  )
                , remaining_
                )


hitTest : BoundingBox -> BoundingBox -> Bool
hitTest target primary =
    case primary |> Primary |> HitTest.detect (Target target) of
        Miss ->
            False

        Resting _ ->
            False

        _ ->
            True



-- REBUILDING


maybeRebuild : Pruner b -> Pruner b
maybeRebuild pruner =
    case pruner.hit of
        Miss ->
            pruner

        _ ->
            pruner
                |> rebuild


rebuild : Pruner b -> Pruner b
rebuild pruner =
    { pruner
        | targetRows =
            pruner
                |> rebuildRows
                |> rebuildTargetRows
    }


rebuildRows : Pruner b -> List (Row b)
rebuildRows pruner =
    case pruner.targetRowHitRemaining of
        Just row ->
            ( ( row
                    |> targets
                    |> List.length
              , row
                    |> targets
                    |> List.map
                        .boundingBox
                    |> BoundingBox.fromList
              )
            , row
                |> targets
            )
                :: pruner.targetRowsMissed

        Nothing ->
            pruner.targetRowsMissed


rebuildTargetRows : List (Row b) -> Rows b
rebuildTargetRows rows_ =
    ( ( rows_
            |> List.foldl
                (\row acc ->
                    row
                        |> rowRemaining
                        |> (+) acc
                )
                0
      , rows_
            |> List.map
                rowBoundingBox
            |> BoundingBox.fromList
      )
    , rows_
    )
