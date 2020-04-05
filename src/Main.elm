module Main exposing (main)

{-| Currently, this is just a wrapper around the
[Space Invaders Game](SpaceInvaders.Game).

The reason it exists, is so that it can become a landing page for multiple
games, with each new game utilizing the modules in the `Shared` folder.

@docs main

-}

import Browser
import Element
import Html exposing (Html)
import SpaceInvaders.Game as SpaceInvaders



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( SpaceInvaders
        (SpaceInvaders.init
            flags
        )
    , Cmd.none
    )


{-| A type alias representing the data required by [init](#init).
-}
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



-- MODEL


type Model
    = SpaceInvaders SpaceInvaders.Model



-- Update


type Msg
    = SpaceInvadersMsg SpaceInvaders.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( SpaceInvadersMsg spaceInvadersMsg, SpaceInvaders spaceInvaders ) ->
            let
                ( game, cmd ) =
                    spaceInvaders
                        |> SpaceInvaders.update
                            spaceInvadersMsg
            in
            ( SpaceInvaders game
            , cmd
                |> Cmd.map
                    SpaceInvadersMsg
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (SpaceInvaders spaceInvaders) =
    spaceInvaders
        |> SpaceInvaders.subscriptions
        |> Sub.map
            SpaceInvadersMsg



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        []
        (case model of
            SpaceInvaders spaceInvaders ->
                spaceInvaders
                    |> SpaceInvaders.view
                    |> Element.map
                        SpaceInvadersMsg
        )



-- PROGRAM


{-| -}
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
