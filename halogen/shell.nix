{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  serve = writeScriptBin "serve" ''
    npm run serve
  '';

in mkShell {

  nativeBuildInputs = with elmPackages; [
    purescript
    spago
    nodejs
    nodePackages.parcel-bundler
  ];

  buildInputs = [ serve ];
}
