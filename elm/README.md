# Elm

- Gemakkelijke installatie
- `elm init` is erg user friendly
- Idem voor `elm reactor`
- `elm-live` is handig om live code changes te hebben
- `elm-format` is goed om een algemene style guide te hebben
- Het typesystem van Elm is minder uitgebreid dan dit van Haskell (typeclasses)
  - Het gebrek aan typeclasses/generics kan problematisch zijn voor onze use case
- Lijkt enorm op Miso (of eerder omgekeerd eigenlijk)

## Usage

Development:

```
$ nix-shell --run 'code .' # Liefst met Elm extension
$ nix-shell --run run-elm-live # Live code reloading
```

View your site at http://localhost:8000/
