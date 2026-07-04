# Space Invaders

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

## Purpose

(In no particular order)

1. An experiment to see if I could mimick the classic original video game.

2. To delve into game development with Elm.

3. To improve my Elm skills in general.

## How To Play

```bash
git clone https://github.com/phollyer/elm-space-invaders.git
cd elm-space-invaders
./make.sh
elm reactor
```
Then navigate to `public/index.html`

## Note

Project manifests:

1. `elm.json` is the required Elm 0.19.1 application manifest used by `elm make` and `elm reactor`.
2. `package.json` is optional and only for Node-based JavaScript post-processing workflows (for example minification).

The legacy `elm-application.json` file has been removed because it is not used by Elm 0.19 tooling.

Revival cleanup status:

1. Legacy localDataStorage wiring has been removed.
2. Highscore reads and writes now route through Elm HTTP calls in `SpaceInvaders.Highscores`.
3. JavaScript is no longer responsible for highscore persistence.
4. Each game-over now inserts only the finished score; the client then reloads the top 10 from the backend.

Modern minification options if you want a release bundle:

1. `elm make src/Main.elm --optimize --output public/elm.js`
2. Minify with `terser` (recommended) or `esbuild`.
3. Optionally gzip or brotli the output for deployment.

Build commands:

1. `npm run build`
2. `npm run build:opt`

Supabase migration next steps:

1. Configure `cache.readEndpoint`, `cache.writeEndpoint`, and `cache.anonKey` in `public/index.html`.
2. Use query parameters only on `cache.readEndpoint`, and keep `cache.writeEndpoint` as the base table REST endpoint.
3. Leave `cache.accessToken` empty for a public scoreboard, or provide a real Supabase Auth session token for authenticated requests.
4. Decide conflict behavior for equal scores (overwrite, first write wins, or keep oldest).

Supabase schema setup:

1. A migration has been added at `supabase/migrations/20260703224956_create_highscores_table.sql`.
2. Apply it with `supabase db push`.
3. The migration creates `public.highscores` and public `select` plus `insert` RLS policies for the `anon` role.

Backend error visibility:

1. If loading highscores fails, a `Backend error:` message is shown in the UI.
2. If saving highscores fails at game-over, a `Backend error:` message is shown in the UI.

Regarding Touch devices, I have tested this on iPhone 7 and an old iPad Mini. It runs fine on the iPhone, but exceptionally slow, to the point of being unplayable on the iPad Mini.

The nature of the game also means it is easier to play with a keyboard, than on a Touch device.

## TODO

Change the SVG graphics to more resemble the original, with smaller Invaders on the higher rows etc.

## Credit

SVG Game Graphics and sound effects: <https://github.com/gege251/space_invaders>
