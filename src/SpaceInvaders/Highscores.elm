module SpaceInvaders.Highscores exposing (Config, loadHighscores, saveHighscore)

import Http
import Json.Decode as JD
import Json.Encode as JE


{-| Highscore HTTP boundary.

This module uses HTTP to read and write highscores.

@docs Config, loadHighscores, saveHighscore

-}
type alias Config =
    { readEndpoint : String
    , writeEndpoint : String
    , anonKey : String
    }


headers : Config -> List Http.Header
headers config =
    [ Http.header "apikey" config.anonKey
    , Http.header "content-type" "application/json"
    ]


highscoreDecoder : JD.Decoder { name : String, points : Int }
highscoreDecoder =
    JD.map2
        (\name points ->
            { name = name
            , points = points
            }
        )
        (JD.field "name" JD.string)
        (JD.field "points" JD.int)


highscoresDecoder : JD.Decoder (List { name : String, points : Int })
highscoresDecoder =
    JD.list highscoreDecoder


encodeHighscore : { name : String, points : Int } -> JE.Value
encodeHighscore score =
    JE.object
        [ ( "name", JE.string score.name )
        , ( "points", JE.int score.points )
        ]


{-| Load highscores from the configured HTTP endpoint.
-}
loadHighscores : Config -> (Result Http.Error (List { name : String, points : Int }) -> msg) -> Cmd msg
loadHighscores config toMsg =
    Http.request
        { method = "GET"
        , headers = headers config
        , url = config.readEndpoint
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg highscoresDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Save highscores.

This sends a single finished score to the configured HTTP endpoint.

-}
saveHighscore : Config -> (Result Http.Error () -> msg) -> { name : String, points : Int } -> Cmd msg
saveHighscore config toMsg highscore =
    Http.request
        { method = "POST"
        , headers = headers config
        , url = config.writeEndpoint
        , body = Http.jsonBody (encodeHighscore highscore)
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }
