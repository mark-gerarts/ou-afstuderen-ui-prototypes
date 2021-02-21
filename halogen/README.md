# PureScript + Halogen

- Tot nu toe is de werking heel eenvoudig: `spago init` zet veel klaar.
- De starter template (deze dus) heeft zelfs live reloading ingebakken
- Het opbouwen van HTML en events etc lijkt enorm op Elm en Miso
- Krachtiger dan Elm (typeclasses etc)
- Het feit dat het erg op Haskell lijkt is een beetje dubbel: aan de ene kant
  kun je snel concepten overnemen, maar vaak zijn ze net dat beetje anders qua
  syntax of werking.

## Usage

Compiles the project, starts a server, opens your browser and hot reloads the
code:

```
$ nix-shell --run serve
```

For more information, see the template's repository:
https://github.com/purescript-halogen/purescript-halogen-template

Run vscode the usual way, to get highlighting and formatting etc:

```
$ nix-shell --run 'code .'
```

Extension: nwolverson.ide-purescript
