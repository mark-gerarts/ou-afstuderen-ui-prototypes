# Miso

## Bemerkingen

- De installatie met Nix is niet zo straightforward.
- De quickstart commands werken niet (loopt vast op `nix-shell -A env`) (fixed)
- Documentatie: link naar voorbeeld client/server geeft een 404 (wat wel nuttig was voor ons)
- Het voorbeeld met JSaddle heeft geen live reload zoals advertised
  - Ik ben niet de enige met dit probleem: https://github.com/dmjio/miso/issues/642
- onSubmit met preventDefault lijkt niet te werken - al veel tijd in gestoken zonder oplossing
  - In Functional Programming Slack om hulp gevraagd (30/01 - nog geen antwoord)
  - Blijkt enkel in JSaddle een probleem te zijn
  - Tijdelijk opgelost door een div ipv een form te gebruiken
- Rechtstreeks Haskell kunnen gebruiken is wel een groot voordeel
  - Autocomplete, intellisense etc in vscode werkt niet out-of-the-box zoals bij een standaard Stack project
  - Dit deelt Miso met Reflex. Beide gebruiken GHCJS, wat moeilijk is om te installeren en te integreren in de editor
- Werking doet aan Elm denken, eenvoudig en vlot
- Debuggen is niet gemakkelijk
- Het is niet eenvoudig mogelijk om JSaddle te combineren met een XHR request (ghcjs vs ghc) -> toch maar afstappen van JSaddle
  - Als ik ooit terug wil overgaan is hier wat uitleg https://github.com/dmjio/miso/issues/594. Het is vooral gewoon gek dat dit niet out-of-the-box werkt.
- Het gebruik van Nix wordt aangeraden, maar dit heeft een heel grote leercurve - komend van iemand die NixOS gebruikt.
- Heel gek: in het scenario dat er in `updateModel` een case ontbreekt, komt de compiler niet klagen. Maar: de app werkt dan niet omdat er bij startup een JS error in de console komt.

## Usage

The current workflow: we nix-shell one time to get dependencies such as cabal,
entr, ... The Miso manual installs these globally, which we don't want.

Inside this shell we run nix-shell again to rebuild on file changes, as
described in the manual.

```
$ nix-shell
$ nix-shell default.nix -A env --run 'ag -l | entr sh -c "cabal build"'
```

The result can be found in the `dist-newstyle` directory. To test it out, don't
forget to start the [webserver](../server/README.md).

This was the old JSaddle workflow - which sadly is no longer relevant, because
it can't be (easily) combined with XHR requests. It was accessible at
`localhost:8080`.

```
$ # JSaddle dev:
$ nix-shell --run reload
$ # Build
$ nix-build
```
