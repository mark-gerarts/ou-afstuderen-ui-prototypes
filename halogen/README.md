# PureScript + Halogen

- Tot nu toe is de werking heel eenvoudig: `spago init` zet veel klaar.
- De starter template (deze dus) heeft zelfs live reloading ingebakken
- Het opbouwen van HTML en events etc lijkt enorm op Elm en Miso
- Krachtiger dan Elm (typeclasses etc)
- Het feit dat het erg op Haskell lijkt is een beetje dubbel: aan de ene kant
  kun je snel concepten overnemen, maar vaak zijn ze net dat beetje anders qua
  syntax of werking.
- Ik ervaar het als moeilijk om dingen te implementeren, zeker in vergelijking
  met Elm.
  - Die moeilijkheid kwam vooral uit het bekijken van de [verkeerde
    documentatie](https://github.com/purescript-halogen/purescript-halogen/pull/730)...
    Met de juiste docs erbij valt het allemaal goed mee.
- [Components](https://purescript-halogen.github.io/purescript-halogen/guide/05-Parent-Child-Components.html)
  zijn zeer krachtig om separation of concerns te bereiken.
  - Ik heb het prototype herwerkt om hier gebruik van te maken
  - Voor dit formaat applicatie is het overkill, maar het toont wel aan hoe de
    state en afhandeling per component een pak eenvoudiger wordt
  - Gedeelde code kan natuurlijk nog altijd, zoals bv `Album.purs`

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
