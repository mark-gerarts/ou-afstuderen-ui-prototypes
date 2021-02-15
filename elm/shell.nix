{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  run-elm-live = writeScriptBin "run-elm-live" ''
    elm-live src/Main.elm -s main.html -- --output=main.js
  '';

in mkShell {

  nativeBuildInputs = with elmPackages; [
    elm
    elm-live
    elm-format
  ];

  buildInputs = [ run-elm-live ];
}
