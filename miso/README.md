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

## Usage

```
$ $ JSaddle dev:
$ nix-shell --run reload
$ # Build
$ nix-build
```
