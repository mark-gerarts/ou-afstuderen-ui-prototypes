# Elm

- Gemakkelijke installatie
- `elm init` is erg user friendly
- Idem voor `elm reactor`
- `elm-live` is handig om live code changes te hebben
- `elm-format` is goed om een algemene style guide te hebben
- Het typesystem van Elm is minder uitgebreid dan dit van Haskell (typeclasses)
- Lijkt enorm op Miso (of eerder omgekeerd eigenlijk)

## Usage

Development:

```
$ nix-shell --run 'code .' & # Liefst met Elm extension
$ nix-shell
$ elm-live src/Main.elm -s main.html -- --output=main.js
```

View your site at http://localhost:8000/
