module Shared.HitTest exposing
    ( Primary(..), Target(..)
    , detectAny
    , Edge(..)
    , Speared(..)
    , Overlap(..)
    , Hit(..), detect, detectOnly, detectOneOf
    )

{-| Detect if two [BoundingBox](Shared.BoundingBox#BoundingBox)es hit each
other.

This module works with the notion of a [Primary](#Primary) and a
[Target](#Target). How important these are, really depends on how complex your
game is with respect to your [Hit](#Hit) tests.


# SIMPLE DETECTING

For instance, in a simple shoot 'em up type game, where all you need to know is
if two [BoundingBox](Shared.BoundingBox#BoundingBox)'s collide, it does not
matter which [BoundingBox](Shared.BoundingBox#BoundingBox) you define as the
[Primary](#Primary), and which as the [Target](#Target).

@docs Primary, Target

@docs detectAny


# ADVANCED DETECTING

In more complex games, however, where knowing **how** two objects collide is
important, then the relationship between [Primary](#Primary) and
[Target](#Target) also becomes important.

For example, let's say we have a platform game in which the player has to move
up by climbing ladders, but the player can't climb a ladder unless they are
directly in front of it.

In this case, we probably need to know if the player's
[BoundingBox](Shared.BoundingBox#BoundingBox) is inside the ladder's
[BoundingBox](Shared.BoundingBox#BoundingBox). To do this, we co do something
like the following:

    Primary player
        |> detectOnly Inside (Target ladder)

However, getting the [Primary](#Primary) and the [Target](#Target) the wrong
way round when you are testing for specific [Hit](#Hit) types, will produce
unexpected results. In this case, if `player` and `ladder` were switched, the
result is likely to always be `False`:

    Primary ladder
        |> detectOnly Inside (Target player)
    -- If the ladder is wider than the player, this can never be True

Switching [Inside](#Hit) to [Covers](#Hit), reads better, and would also be a
fix - as would reverting `player` and `ladder` back to their original contexts,
as follows:

    -- This is the same hit test
    Primary ladder
        |> detectOnly Covers (Target player)

    -- as this
    Primary player
        |> detectOnly Inside (Target ladder)

For a visual view of the relationship between [Primary](#Primary) and
[Target](#Target) take a look at
[HitTest.pdf](https://github.com/phollyer/elm-space-invaders/blob/master/tests/Shared/HitTest.pdf).

@docs Edge

@docs Speared

@docs Overlap

@docs Hit, detect, detectOnly, detectOneOf

-}

import Shared.BoundingBox as BoundingBox exposing (BoundingBox)


{-| A type representing the primary [BoundingBox](Shared.BoundingBox#Box).
-}
type Primary
    = Primary BoundingBox


{-| A type representing the target [BoundingBox](Shared.BoundingBox#Box).
-}
type Target
    = Target BoundingBox


{-| A union type, representing the edges of a
[BoundingBox](Shared.BoundingBox#BoundingBox).
-}
type Edge
    = Left
    | Top
    | Right
    | Bottom


{-| A union type, representing ways in which the [Primary](#Primary) spears
the [Target](#Target).
-}
type Speared
    = LeftOrRight Edge
    | LeftAndRightOutside
    | LeftAndRightMatch
    | TopOrBottom Edge
    | TopAndBottomOutside
    | TopAndBottomMatch


{-| A union type, representing ways in which the [Primary](#Primary) overlaps
the [Target](#Target).
-}
type Overlap
    = Edge Edge
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


{-| A union type, representing the different ways in which a
[Primary](#Primary) [BoundingBox](Shared.BoundingBox#BoundingBox) can
[Hit](#Hit) a [Target](#Target) [BoundingBox](Shared.BoundingBox#BoundingBox).
-}
type Hit
    = Miss
    | Exact
    | Inside
    | Covers
    | Speared Speared
    | Overlap Overlap
    | Resting Edge


{-| Detect if the [Primary](#Primary)
[BoundingBox](Shared.BoundingBox#BoundingBox) hits the [Target](#Target)
[BoundingBox](Shared.BoundingBox#BoundingBox), and return the [Hit](#Hit) type.

    import Shared.BoundingBox as BoundingBox

    Primary
        (BoundingBox.init
            { left = 10
            , top = 10
            , right = 10
            , bottom = 20
            }
        )
        |> detect
            (Target
                (BoundingBox.init
                    { left = 5
                    , top = 5
                    , right = 15
                    , bottom = 15
                    }
                )
            )
        --> (Speared (TopOrBottom Bottom))

-}
detect : Target -> Primary -> Hit
detect (Target target) (Primary primary) =
    let
        ( _, maybeHit, _ ) =
            ( primary, Nothing, target )
                |> detectMiss
                |> detectExact
                |> detectInside
                |> detectResting
                |> detectSpeared
                |> detectCovers
                |> detectOverlap
    in
    case maybeHit of
        Nothing ->
            Miss

        Just hit ->
            hit


{-| Detect if the [Primary](#Primary)
[BoundingBox](Shared.BoundingBox#BoundingBox) hits the [Target](#Target)
[BoundingBox](Shared.BoundingBox#BoundingBox) by only the specified [Hit](#Hit)
type.

    import Shared.BoundingBox as BoundingBox

    Primary
        (BoundingBox.init
            { left = 10
            , top = 10
            , right = 10
            , bottom = 20
            }
        )
        |> detectOnly
            (Overlap (Edge Bottom))
            (Target
                (BoundingBox.init
                    { left = 5
                    , top = 5
                    , right = 15
                    , bottom = 15
                    }
                )
            )
        --> True

-}
detectOnly : Hit -> Target -> Primary -> Bool
detectOnly hit (Target target) (Primary primary) =
    case hit of
        Miss ->
            ( primary, Nothing, target )
                |> detectMiss
                |> matchOnly
                    Miss

        Exact ->
            ( primary, Nothing, target )
                |> detectExact
                |> matchOnly
                    Exact

        Inside ->
            ( primary, Nothing, target )
                |> detectInside
                |> matchOnly
                    Inside

        Speared speared ->
            ( primary, Nothing, target )
                |> detectSpeared
                |> matchOnly
                    (Speared speared)

        Covers ->
            ( primary, Nothing, target )
                |> detectCovers
                |> matchOnly
                    Covers

        Resting edge ->
            ( primary, Nothing, target )
                |> detectResting
                |> matchOnly
                    (Resting edge)

        Overlap overlap ->
            ( primary, Nothing, target )
                |> detectOverlap
                |> matchOnly
                    (Overlap overlap)


matchOnly : Hit -> ( BoundingBox, Maybe Hit, BoundingBox ) -> Bool
matchOnly hit ( _, maybeHit, _ ) =
    case maybeHit of
        Nothing ->
            False

        Just hit_ ->
            hit == hit_


{-| Detect if the [Primary](#Primary)
[BoundingBox](Shared.BoundingBox#BoundingBox) hits the [Target](#Target)
[BoundingBox](Shared.BoundingBox#BoundingBox) by any one of the specified
[Hit](#Hit) types.

    import Shared.BoundingBox as BoundingBox

    Primary
        (BoundingBox.init
            { left = 10
            , top = 10
            , right = 10
            , bottom = 20
            }
        )
        |> detectOneOf
            [Resting Left, Overlap (Edge Bottom), Resting Right]
            (Target
                (BoundingBox.init
                    { left = 5
                    , top = 5
                    , right = 15
                    , bottom = 15
                    }
                )
            )
        --> True

-}
detectOneOf : List Hit -> Target -> Primary -> Bool
detectOneOf hits target primary =
    case hits of
        [] ->
            False

        first :: rest ->
            case primary |> detectOnly first target of
                True ->
                    True

                False ->
                    primary
                        |> detectOneOf
                            rest
                            target


{-| Detect any sort of [Hit](#Hit) between two
[BoundingBox](Shared.BoundingBox#BoundingBox)es.

    import Shared.BoundingBox as BoundingBox

    Primary
        (BoundingBox.init
            { left = 10
            , top = 10
            , right = 10
            , bottom = 20
            }
        )
        |> detectAny
            (Target
                (BoundingBox.init
                    { left = 5
                    , top = 5
                    , right = 15
                    , bottom = 15
                    }
                )
            )
        --> True

A `Miss` will return `False`.

    import Shared.BoundingBox as BoundingBox

    Primary
        (BoundingBox.init
            { left = 20
            , top = 20
            , right = 30
            , bottom = 30
            }
        )
        |> detectAny
            (Target
                (BoundingBox.init
                    { left = 5
                    , top = 5
                    , right = 15
                    , bottom = 15
                    }
                )
            )
        --> False

-}
detectAny : Target -> Primary -> Bool
detectAny target primary =
    case primary |> detect target of
        Miss ->
            False

        _ ->
            True


detectMiss : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectMiss ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    Miss
                    miss
                    target

        _ ->
            ( primary, maybeHit, target )


detectExact : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectExact ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    Exact
                    exactHit
                    target

        _ ->
            ( primary, maybeHit, target )


detectInside : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectInside ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    Inside
                    allInside
                    target

        _ ->
            ( primary, maybeHit, target )


detectSpeared : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectSpeared ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            ( primary, maybeHit, target )
                |> detectSpearedBottom
                |> detectSpearedTop
                |> detectSpearedLeft
                |> detectSpearedRight
                |> detectSpearedTopAndBottomOutside
                |> detectSpearedLeftAndRightOutside
                |> detectSpearedTopAndBottomMatch
                |> detectSpearedLeftAndRightMatch

        _ ->
            ( primary, maybeHit, target )


detectSpearedLeft : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectSpearedLeft ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Speared
                        (LeftOrRight Left)
                    )
                    spearedLeft
                    target

        _ ->
            ( primary, maybeHit, target )


detectSpearedLeftAndRightMatch : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectSpearedLeftAndRightMatch ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Speared LeftAndRightMatch)
                    spearedLeftAndRightMatch
                    target

        _ ->
            ( primary, maybeHit, target )


detectSpearedLeftAndRightOutside : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectSpearedLeftAndRightOutside ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Speared LeftAndRightOutside)
                    spearedLeftAndRightOutside
                    target

        _ ->
            ( primary, maybeHit, target )


detectSpearedRight : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectSpearedRight ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Speared
                        (LeftOrRight Right)
                    )
                    spearedRight
                    target

        _ ->
            ( primary, maybeHit, target )


detectSpearedTop : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectSpearedTop ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Speared
                        (TopOrBottom Top)
                    )
                    spearedTop
                    target

        _ ->
            ( primary, maybeHit, target )


detectSpearedTopAndBottomOutside : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectSpearedTopAndBottomOutside ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Speared TopAndBottomOutside)
                    spearedTopAndBottomOutside
                    target

        _ ->
            ( primary, maybeHit, target )


detectSpearedTopAndBottomMatch : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectSpearedTopAndBottomMatch ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Speared TopAndBottomMatch)
                    spearedTopAndBottomMatch
                    target

        _ ->
            ( primary, maybeHit, target )


detectSpearedBottom : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectSpearedBottom ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Speared
                        (TopOrBottom Bottom)
                    )
                    spearedBottom
                    target

        _ ->
            ( primary, maybeHit, target )


detectCovers : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectCovers ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            ( primary, Nothing, target )
                |> detectAllOutside

        _ ->
            ( primary, maybeHit, target )


detectAllOutside : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectAllOutside ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    Covers
                    allOutside
                    target

        _ ->
            ( primary, maybeHit, target )


detectResting : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectResting ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            ( primary, maybeHit, target )
                |> detectRestingLeft
                |> detectRestingTop
                |> detectRestingRight
                |> detectRestingBottom

        _ ->
            ( primary, maybeHit, target )


detectRestingLeft : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectRestingLeft ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Resting Left)
                    restingLeft
                    target

        _ ->
            ( primary, maybeHit, target )


detectRestingTop : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectRestingTop ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Resting Top)
                    restingTop
                    target

        _ ->
            ( primary, maybeHit, target )


detectRestingRight : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectRestingRight ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Resting Right)
                    restingRight
                    target

        _ ->
            ( primary, maybeHit, target )


detectRestingBottom : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectRestingBottom ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Resting Bottom)
                    restingBottom
                    target

        _ ->
            ( primary, maybeHit, target )


detectOverlap : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectOverlap ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            ( primary, Nothing, target )
                |> detectOverlapLeft
                |> detectOverlapRight
                |> detectOverlapTop
                |> detectOverlapBottom
                |> detectOverlapTopLeft
                |> detectOverlapTopRight
                |> detectOverlapBottomLeft
                |> detectOverlapBottomRight

        _ ->
            ( primary, maybeHit, target )


detectOverlapLeft : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectOverlapLeft ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Overlap
                        (Edge Left)
                    )
                    overlapLeft
                    target

        _ ->
            ( primary, maybeHit, target )


detectOverlapRight : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectOverlapRight ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Overlap
                        (Edge Right)
                    )
                    overlapRight
                    target

        _ ->
            ( primary, maybeHit, target )


detectOverlapTop : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectOverlapTop ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Overlap
                        (Edge Top)
                    )
                    overlapTop
                    target

        _ ->
            ( primary, maybeHit, target )


detectOverlapBottom : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectOverlapBottom ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Overlap
                        (Edge Bottom)
                    )
                    overlapBottom
                    target

        _ ->
            ( primary, maybeHit, target )


detectOverlapTopLeft : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectOverlapTopLeft ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Overlap TopLeft)
                    overlapTopLeft
                    target

        _ ->
            ( primary, maybeHit, target )


detectOverlapTopRight : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectOverlapTopRight ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Overlap TopRight)
                    overlapTopRight
                    target

        _ ->
            ( primary, maybeHit, target )


detectOverlapBottomLeft : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectOverlapBottomLeft ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Overlap BottomLeft)
                    overlapBottomLeft
                    target

        _ ->
            ( primary, maybeHit, target )


detectOverlapBottomRight : ( BoundingBox, Maybe Hit, BoundingBox ) -> ( BoundingBox, Maybe Hit, BoundingBox )
detectOverlapBottomRight ( primary, maybeHit, target ) =
    case maybeHit of
        Nothing ->
            primary
                |> checkFor
                    (Overlap BottomRight)
                    overlapBottomRight
                    target

        _ ->
            ( primary, maybeHit, target )


checkFor : Hit -> (BoundingBox -> BoundingBox -> Bool) -> BoundingBox -> BoundingBox -> ( BoundingBox, Maybe Hit, BoundingBox )
checkFor hit func target primary =
    case primary |> func target of
        True ->
            ( primary, Just hit, target )

        False ->
            ( primary, Nothing, target )


miss : BoundingBox -> BoundingBox -> Bool
miss target primary =
    ((primary |> BoundingBox.right) < BoundingBox.left target)
        || ((primary |> BoundingBox.bottom) < BoundingBox.top target)
        || ((primary |> BoundingBox.left) > BoundingBox.right target)
        || ((primary |> BoundingBox.top) > BoundingBox.bottom target)


exactHit : BoundingBox -> BoundingBox -> Bool
exactHit target primary =
    (primary |> leftEdgeMatches target)
        && (primary |> topEdgeMatches target)
        && (primary |> rightEdgeMatches target)
        && (primary |> bottomEdgeMatches target)


allInside : BoundingBox -> BoundingBox -> Bool
allInside target primary =
    (primary |> leftEdgeInside target)
        && (primary |> topEdgeInside target)
        && (primary |> rightEdgeInside target)
        && (primary |> bottomEdgeInside target)


allOutside : BoundingBox -> BoundingBox -> Bool
allOutside target primary =
    ((primary |> BoundingBox.left) <= BoundingBox.left target)
        && ((primary |> BoundingBox.top) <= BoundingBox.top target)
        && ((primary |> BoundingBox.right) >= BoundingBox.right target)
        && ((primary |> BoundingBox.bottom) >= BoundingBox.bottom target)


spearedBottom : BoundingBox -> BoundingBox -> Bool
spearedBottom target primary =
    (primary |> leftAndRightInside target)
        && ((primary |> topEdgeMatchesBottomEdgeOutside target)
                || (primary |> topEdgeInsideBottomEdgeOutside target)
                || (primary |> topEdgeInsideBottomEdgeMatches target)
           )


spearedLeft : BoundingBox -> BoundingBox -> Bool
spearedLeft target primary =
    (primary |> topAndBottomInside target)
        && ((primary |> leftEdgeOutsideRightEdgeMatches target)
                || (primary |> leftEdgeOutsideRightEdgeInside target)
                || (primary |> leftEdgeMatchesRightEdgeInside target)
           )


spearedLeftAndRightMatch : BoundingBox -> BoundingBox -> Bool
spearedLeftAndRightMatch target primary =
    (primary |> topAndBottomInside target)
        && (primary |> leftAndRightMatch target)


spearedLeftAndRightOutside : BoundingBox -> BoundingBox -> Bool
spearedLeftAndRightOutside target primary =
    (primary |> topAndBottomInside target)
        && (primary |> leftAndRightOutside target)


spearedRight : BoundingBox -> BoundingBox -> Bool
spearedRight target primary =
    (primary |> topAndBottomInside target)
        && ((primary |> leftEdgeMatchesRightEdgeOutside target)
                || (primary |> leftEdgeInsideRightEdgeOutside target)
                || (primary |> leftEdgeInsideRightEdgeMatches target)
           )


spearedTop : BoundingBox -> BoundingBox -> Bool
spearedTop target primary =
    (primary |> leftAndRightInside target)
        && ((primary |> topEdgeOutsideBottomEdgeMatches target)
                || (primary |> topEdgeOutsideBottomEdgeInside target)
                || (primary |> topEdgeMatchesBottomEdgeInside target)
           )


spearedTopAndBottomOutside : BoundingBox -> BoundingBox -> Bool
spearedTopAndBottomOutside target primary =
    (primary |> leftAndRightInside target)
        && (primary |> topAndBottomOutside target)


spearedTopAndBottomMatch : BoundingBox -> BoundingBox -> Bool
spearedTopAndBottomMatch target primary =
    (primary |> leftAndRightInside target)
        && (primary |> topAndBottomMatch target)


overlapLeft : BoundingBox -> BoundingBox -> Bool
overlapLeft target primary =
    ((primary |> rightEdgeInside target)
        && (primary |> leftEdgeInside target |> not)
    )
        && (((primary |> topEdgeInside target)
                && (primary |> bottomEdgeInside target)
            )
                || ((primary |> topEdgeInside target |> not)
                        && (primary |> bottomEdgeInside target |> not)
                   )
           )


overlapRight : BoundingBox -> BoundingBox -> Bool
overlapRight target primary =
    ((primary |> leftEdgeInside target)
        && (primary |> rightEdgeInside target |> not)
    )
        && (((primary |> topEdgeInside target)
                && (primary |> bottomEdgeInside target)
            )
                || ((primary |> topEdgeInside target |> not)
                        && (primary |> bottomEdgeInside target |> not)
                   )
           )


overlapTop : BoundingBox -> BoundingBox -> Bool
overlapTop target primary =
    ((primary |> bottomEdgeInside target)
        && (primary |> topEdgeInside target |> not)
    )
        && (((primary |> leftEdgeInside target)
                && (primary |> rightEdgeInside target)
            )
                || ((primary |> leftEdgeInside target |> not)
                        && (primary |> rightEdgeInside target |> not)
                   )
           )


overlapBottom : BoundingBox -> BoundingBox -> Bool
overlapBottom target primary =
    ((primary |> topEdgeInside target)
        && (primary |> bottomEdgeInside target |> not)
    )
        && (((primary |> leftEdgeInside target)
                && (primary |> rightEdgeInside target)
            )
                || ((primary |> leftEdgeInside target |> not)
                        && (primary |> rightEdgeInside target |> not)
                   )
           )


overlapTopLeft : BoundingBox -> BoundingBox -> Bool
overlapTopLeft target primary =
    (primary |> rightEdgeInside target)
        && (primary |> bottomEdgeInside target)
        && (primary |> leftEdgeInside target |> not)
        && (primary |> topEdgeInside target |> not)


overlapTopRight : BoundingBox -> BoundingBox -> Bool
overlapTopRight target primary =
    (primary |> leftEdgeInside target)
        && (primary |> bottomEdgeInside target)
        && (primary |> rightEdgeInside target |> not)
        && (primary |> topEdgeInside target |> not)


overlapBottomLeft : BoundingBox -> BoundingBox -> Bool
overlapBottomLeft target primary =
    (primary |> rightEdgeInside target)
        && (primary |> topEdgeInside target)
        && (primary |> leftEdgeInside target |> not)
        && (primary |> bottomEdgeInside target |> not)


overlapBottomRight : BoundingBox -> BoundingBox -> Bool
overlapBottomRight target primary =
    (primary |> leftEdgeInside target)
        && (primary |> topEdgeInside target)
        && (primary |> rightEdgeInside target |> not)
        && (primary |> bottomEdgeInside target |> not)


bottomEdgeInside : BoundingBox -> BoundingBox -> Bool
bottomEdgeInside target primary =
    ((primary |> BoundingBox.bottom) > BoundingBox.top target)
        && ((primary |> BoundingBox.bottom) < BoundingBox.bottom target)


bottomEdgeMatches : BoundingBox -> BoundingBox -> Bool
bottomEdgeMatches target primary =
    (primary |> BoundingBox.bottom) == BoundingBox.bottom target


bottomEdgeOutside : BoundingBox -> BoundingBox -> Bool
bottomEdgeOutside target primary =
    (primary |> BoundingBox.bottom) > BoundingBox.bottom target


leftAndRightInside : BoundingBox -> BoundingBox -> Bool
leftAndRightInside target primary =
    (primary |> leftEdgeInside target)
        && (primary |> rightEdgeInside target)


leftAndRightMatch : BoundingBox -> BoundingBox -> Bool
leftAndRightMatch target primary =
    (primary |> leftEdgeMatches target)
        && (primary |> rightEdgeMatches target)


leftAndRightOutside : BoundingBox -> BoundingBox -> Bool
leftAndRightOutside target primary =
    (primary |> leftEdgeOutside target)
        && (primary |> rightEdgeOutside target)


leftEdgeInside : BoundingBox -> BoundingBox -> Bool
leftEdgeInside target primary =
    ((primary |> BoundingBox.left) > BoundingBox.left target)
        && ((primary |> BoundingBox.left) < BoundingBox.right target)


leftEdgeInsideRightEdgeMatches : BoundingBox -> BoundingBox -> Bool
leftEdgeInsideRightEdgeMatches target primary =
    (primary |> leftEdgeInside target)
        && (primary |> rightEdgeMatches target)


leftEdgeInsideRightEdgeOutside : BoundingBox -> BoundingBox -> Bool
leftEdgeInsideRightEdgeOutside target primary =
    (primary |> leftEdgeInside target)
        && (primary |> rightEdgeOutside target)


leftEdgeMatches : BoundingBox -> BoundingBox -> Bool
leftEdgeMatches target primary =
    (primary |> BoundingBox.left) == BoundingBox.left target


leftEdgeMatchesRightEdgeInside : BoundingBox -> BoundingBox -> Bool
leftEdgeMatchesRightEdgeInside target primary =
    (primary |> leftEdgeMatches target)
        && (primary |> rightEdgeInside target)


leftEdgeMatchesRightEdgeOutside : BoundingBox -> BoundingBox -> Bool
leftEdgeMatchesRightEdgeOutside target primary =
    (primary |> leftEdgeMatches target)
        && (primary |> rightEdgeOutside target)


leftEdgeOutside : BoundingBox -> BoundingBox -> Bool
leftEdgeOutside target primary =
    (primary |> BoundingBox.left) < BoundingBox.left target


leftEdgeOutsideRightEdgeInside : BoundingBox -> BoundingBox -> Bool
leftEdgeOutsideRightEdgeInside target primary =
    (primary |> leftEdgeOutside target)
        && (primary |> rightEdgeInside target)


leftEdgeOutsideRightEdgeMatches : BoundingBox -> BoundingBox -> Bool
leftEdgeOutsideRightEdgeMatches target primary =
    (primary |> leftEdgeOutside target)
        && (primary |> rightEdgeMatches target)


restingLeft : BoundingBox -> BoundingBox -> Bool
restingLeft target primary =
    (primary |> BoundingBox.right) == BoundingBox.left target


restingRight : BoundingBox -> BoundingBox -> Bool
restingRight target primary =
    (primary |> BoundingBox.left) == BoundingBox.right target


restingTop : BoundingBox -> BoundingBox -> Bool
restingTop target primary =
    (primary |> BoundingBox.bottom) == BoundingBox.top target


restingBottom : BoundingBox -> BoundingBox -> Bool
restingBottom target primary =
    (primary |> BoundingBox.top) == BoundingBox.bottom target


rightEdgeInside : BoundingBox -> BoundingBox -> Bool
rightEdgeInside target primary =
    ((primary |> BoundingBox.right) > BoundingBox.left target)
        && ((primary |> BoundingBox.right) < BoundingBox.right target)


rightEdgeMatches : BoundingBox -> BoundingBox -> Bool
rightEdgeMatches target primary =
    (primary |> BoundingBox.right) == BoundingBox.right target


rightEdgeOutside : BoundingBox -> BoundingBox -> Bool
rightEdgeOutside target primary =
    (primary |> BoundingBox.right) > BoundingBox.right target


topAndBottomInside : BoundingBox -> BoundingBox -> Bool
topAndBottomInside target primary =
    (primary |> topEdgeInside target)
        && (primary |> bottomEdgeInside target)


topAndBottomMatch : BoundingBox -> BoundingBox -> Bool
topAndBottomMatch target primary =
    (primary |> topEdgeMatches target)
        && (primary |> bottomEdgeMatches target)


topAndBottomOutside : BoundingBox -> BoundingBox -> Bool
topAndBottomOutside target primary =
    (primary |> topEdgeOutside target)
        && (primary |> bottomEdgeOutside target)


topEdgeInside : BoundingBox -> BoundingBox -> Bool
topEdgeInside target primary =
    ((primary |> BoundingBox.top) > BoundingBox.top target)
        && ((primary |> BoundingBox.top) < BoundingBox.bottom target)


topEdgeInsideBottomEdgeMatches : BoundingBox -> BoundingBox -> Bool
topEdgeInsideBottomEdgeMatches target primary =
    (primary |> topEdgeInside target)
        && (primary |> bottomEdgeMatches target)


topEdgeInsideBottomEdgeOutside : BoundingBox -> BoundingBox -> Bool
topEdgeInsideBottomEdgeOutside target primary =
    (primary |> topEdgeInside target)
        && (primary |> bottomEdgeOutside target)


topEdgeMatches : BoundingBox -> BoundingBox -> Bool
topEdgeMatches target primary =
    (primary |> BoundingBox.top) == BoundingBox.top target


topEdgeMatchesBottomEdgeInside : BoundingBox -> BoundingBox -> Bool
topEdgeMatchesBottomEdgeInside target primary =
    (primary |> topEdgeMatches target)
        && (primary |> bottomEdgeInside target)


topEdgeMatchesBottomEdgeOutside : BoundingBox -> BoundingBox -> Bool
topEdgeMatchesBottomEdgeOutside target primary =
    (primary |> topEdgeMatches target)
        && (primary |> bottomEdgeOutside target)


topEdgeOutside : BoundingBox -> BoundingBox -> Bool
topEdgeOutside target primary =
    (primary |> BoundingBox.top) < BoundingBox.top target


topEdgeOutsideBottomEdgeInside : BoundingBox -> BoundingBox -> Bool
topEdgeOutsideBottomEdgeInside target primary =
    (primary |> topEdgeOutside target)
        && (primary |> bottomEdgeInside target)


topEdgeOutsideBottomEdgeMatches : BoundingBox -> BoundingBox -> Bool
topEdgeOutsideBottomEdgeMatches target primary =
    (primary |> topEdgeOutside target)
        && (primary |> bottomEdgeMatches target)
