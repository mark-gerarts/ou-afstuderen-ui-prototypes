{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {

  nativeBuildInputs = with elmPackages; [
    elm
    elm-live
    elm-format
  ];

  shellHook = ''
  '';
}
