# Miso

## Bemerkingen

- De installatie met Nix is niet zo straightforward.
- De quickstart commands werken niet (loopt vast op `nix-shell -A env`)
- Documentatie: link naar voorbeeld client/server geeft een 404 (wat wel nuttig was voor ons)
- Het voorbeeld met JSaddle heeft geen live reload zoals advertised
- onSubmit met preventDefault lijkt niet te werken - al veel tijd in gestoken zonder oplossing
  - In Functional Programming Slack om hulp gevraagd (30/01 - nog geen antwoord)
  - Blijkt enkel in JSaddle een probleem te zijn
  - Tijdelijk opgelost door een div ipv een form te gebruiken
- Rechtstreeks Haskell kunnen gebruiken is wel een groot voordeel
  - Autocomplete in vscode werkt niet out-of-the-box zoals bij een Stack project
- Werking doet aan Elm denken, eenvoudig en vlot
- Debuggen is niet gemakkelijk
- Het is niet eenvoudig mogelijk om JSaddle te combineren met een XHR request (ghcjs vs ghc) -> toch maar afstappen van JSaddle
- Het gebruik van Nix wordt aangeraden, maar dit heeft een heel grote leercurve - komend van iemand die NixOS gebruikt.

## Usage

The current workflow: we nix-shell one time to get dependencies such as cabal, entr, ... The Miso manual
installs these globally, which we don't want.

Inside this shell we run nix-shell again to rebuild on file changes, as described in the manual.

```
$ nix-shell
$ nix-shell default.nix -A env --run 'ag -l | entr sh -c "cabal build"'
```

The result can be found in the `dist-newstyle` directory.

This was the old JSaddle workflow - which sadly is no longer relevant, because it can't be (easily)
combined with XHR requests. It was accessible at `localhost:8080`.

```
$ # JSaddle dev:
$ nix-shell --run reload
$ # Build
$ nix-build
```
