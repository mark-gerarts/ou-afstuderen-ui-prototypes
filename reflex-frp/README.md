# Reflex FRP

## Remarks

- Reflex is in staat om buiten web ook gtk en mobile applicaties te targetten
- Obelisk is een set tools om Reflex applicaties te ontwikkelen (e.g. built-in
  webserver, hot reloading, etc.)
- Obelisk installatie zoals in de handleiding faalt, openstaand issue:
  - https://github.com/obsidiansystems/obelisk/issues/836
  - https://github.com/obsidiansystems/obelisk/issues/837
  - Opgelost door een oude versie te installeren:
    `nix-env -f https://github.com/obsidiansystems/obelisk/archive/v0.9.0.1.tar.gz -iA command`
- Getting started zegt om na installatie een `ob init` te doen. Dit faalt ook,
  dus obelisk is een dead end. Comment toegevoegd op #836.
- Reflex-platform zelf gaf ook installatie issues, deze zijn wel opgelost door
  een lagere versie te gebruiken (v.0.6.2.0), zoals beschreven in
  https://github.com/reflex-frp/reflex-platform/issues/717
- Een eerste impressie (van alle documentatie door te lezen en tegen installatie
  issues te lopen) is dat Reflex complex is, met veel *moving parts*. Dit maakt
  het potentieel geen goede kandidaat om te gebruiken in een research project
  dat vaak van richting moet veranderen. Ik heb schrik dat je met Reflex vast
  gaat lopen in de dependencies.
- Het is me niet gelukt om mijn editor (vscode) werkend te krijgen met
  intellisense etc...
- Compilen via cabal is nog steeds *traag*. Ik ben ervan overtuigd dat obelisk
  de way to go is, maar totdat het issue is opgelost is het helaas geen optie.
- Documentatie *lijkt* in het begin zeer uitgebreid, maar de verschillende
  bronnen herhalen vaak dezelfde informatie, en er is geen volledige reference
  te vinden.
  - "Browse the haddock docs for examples" -> geen examples
  - Delen van de documentatie outdated (e.g. deprecated MonadWidget)
  - Veel uitzoekwerk is trial & error
- Mogelijks interessante
  [paper](https://futureofcoding.org/papers/comprehensible-frp/comprehensible-frp.pdf),
  en [deze](https://lirias.kuleuven.be/retrieve/466587)
- Hoewel ik denk dat Reflex enorm krachtig is, denk ik ook dat de leercurve te
  groot is voor dit project.

## Useful resources

- [Development setup](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst)
- [Reflex FRP introduction](https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md)
- [Official tutorial](https://reflex-frp.org/tutorial)
- [Examples](https://examples.reflex-frp.org/examples)
- [Main docs](http://docs.reflex-frp.org/en/latest/reflex_docs.html)

## Installation/usage

- Make sure [nix is installed](https://nixos.org/download.html).
- Set up nix caches (see
  [obelisk](https://github.com/obsidiansystems/obelisk#installing-obelisk), 2.I
  or 2.II).
- `git submodule update --init` to fetch reflex-platform
- `reflex-platform/try-reflex` once to install dependencies; exit the shell
  afterwards.

Building the frontend (not advised for development):

```
$ nix-build -o frontend-result -A ghcjs.frontend
```

Building with cabal (faster, preferred for development):

```
$ # This lands you in a reflex powered shell with several build tools available.
$ # See the "development setup" link above for more information
$ nix-shell -A shells.ghcjs
$ # While inside this shell:
$ build # Single build
$ watch # Triggers builds on file changes in the frontend directory
```

The cabal build output can be found in the `dist-ghcjs` directory.
