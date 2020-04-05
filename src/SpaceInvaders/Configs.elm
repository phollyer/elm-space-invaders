module SpaceInvaders.Configs exposing
    ( Ship, ship, Laser, laser
    , Aliens, aliens, AlienLaser, alienLaser
    , Mothership, mothership
    , Bunkers, bunkers
    , Ground, ground
    , View, view
    )

{-| The static configs used throughout the game.


# PLAYER

@docs Ship, ship, Laser, laser


# ALIENS

@docs Aliens, aliens, AlienLaser, alienLaser


# MOTHERSHIP

@docs Mothership, mothership


# BUNKERS

@docs Bunkers, bunkers


# GROUND

@docs Ground, ground


# VIEW

@docs View, view

-}

-- ALIENS


{-| A type alias representing the static configuration settings for the
[Aliens](SpaceInvaders.Assets.Aliens).
-}
type alias Aliens =
    { height : Float
    , width : Float
    , spacing : Float
    , stepDown : Float
    , viewBox : String
    }


{-| -}
aliens : Aliens
aliens =
    { height = 10
    , width = 10
    , spacing = 10
    , stepDown = 5
    , viewBox = "0 0 1000 1000"
    }


{-| A type alias representing the static configuration settings for an
[Alien](SpaceInvaders.Assets.Aliens#Alien) [Laser](SpaceInvaders.Assets.Laser).
-}
type alias AlienLaser =
    { height : Float
    , velocity : Float
    }


{-| -}
alienLaser : AlienLaser
alienLaser =
    { height = 5
    , velocity = 1
    }



-- BUNKERS


{-| A type alias representing the static configuration settings for the
[Bunkers](SpaceInvaders.Assets.Bunkers).
-}
type alias Bunkers =
    { leftX : Float
    , middleX : Float
    , rightX : Float
    , y : Float
    , color : String
    , definition : List (List Int)
    }


{-| -}
bunkers : Bunkers
bunkers =
    let
        totalWidth =
            view.boundaryRight - view.boundaryLeft

        bunkerWidth =
            25

        space =
            (totalWidth - (bunkerWidth * 3)) / 4

        leftX =
            view.boundaryLeft + space

        middleX =
            (totalWidth / 2) - (bunkerWidth / 2) - 0.25

        rightX =
            totalWidth - space - bunkerWidth - 0.5
    in
    { leftX = leftX
    , middleX = middleX
    , rightX = rightX
    , y = ground.y - ship.height - 30
    , color = "antiquewhite"
    , definition = bunkerDefinition
    }


bunkerDefinition : List (List Int)
bunkerDefinition =
    let
        x =
            1
    in
    [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0 ]
    , [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, x, x, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0, 0, x, x, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, x, x, x, x, x, x ]
    , [ x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, x, x, x, x, x, x ]
    ]



-- GROUND


{-| A type alias representing the static configuration settings for the
[Ground](SpaceInvaders.Assets.Ground).
-}
type alias Ground =
    { boundaryLeft : Float
    , boundaryRight : Float
    , y : Float
    }


{-| -}
ground : Ground
ground =
    { boundaryLeft = view.boundaryLeft
    , boundaryRight = view.boundaryRight - 1
    , y = view.boundaryBottom
    }



-- MOTHERSHIP


{-| A type alias representing the static configuration settings for a
[Mothership](SpaceInvaders.Assets.Mothership).
-}
type alias Mothership =
    { y : Float
    , height : Float
    , width : Float
    , laserSpeed : Float
    }


{-| -}
mothership : Mothership
mothership =
    { y = view.boundaryTop
    , height = 10
    , width = 10
    , laserSpeed = 2
    }



-- SHIP


{-| A type alias representing the static configuration settings for a
[Players](Shared.Players) [Ship](SpaceInvaders.Assets.Ship).
-}
type alias Ship =
    { width : Float
    , height : Float
    , viewBox : String
    , velocity : Float
    }


{-| -}
ship : Ship
ship =
    { height = 10
    , width = 9
    , viewBox = "0 0 180 200"
    , velocity = 1
    }



-- LASER


{-| A type alias representing the static configuration settings for a
[Players](Shared.Players) [Laser](SpaceInvaders.Assets.Laser).
-}
type alias Laser =
    { height : Float
    , velocity : Float
    }


{-| -}
laser : Laser
laser =
    { height = 10
    , velocity = 3
    }



-- VIEW


{-| A type alias representing the static configuration settings for the view.
-}
type alias View =
    { boundaryLeft : Float
    , boundaryRight : Float
    , boundaryTop : Float
    , boundaryBottom : Float
    , paddingX : Float
    , paddingY : Float
    , viewBox : String
    }


{-| -}
view : View
view =
    { boundaryLeft = 0
    , boundaryRight = 300
    , boundaryTop = 10
    , boundaryBottom = 190
    , paddingX = 20
    , paddingY = 20
    , viewBox = "0 0 300 200"
    }
