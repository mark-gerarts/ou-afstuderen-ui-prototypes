{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {

  nativeBuildInputs = [
    cabal-install
    ag
    entr
  ];

}
