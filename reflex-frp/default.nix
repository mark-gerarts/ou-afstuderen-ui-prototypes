# See https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst#building-with-nix
# We omitted common + backend since we're only interested in building a frontend
# application.
{ system ? builtins.currentSystem, pkgs ? import <nixpkgs> {} }:
let

  build = pkgs.writeScriptBin "build" ''
    cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all
  '';

  watch = pkgs.writeScriptBin "watch" ''
    ag -l "" frontend/ | entr sh -c "build"
  '';

in (import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {

  packages = {
    frontend = ./frontend;
  };

  shells = {
    ghcjs = ["frontend"];
  };

  shellToolOverrides = self: super: {
    ag = pkgs.ag;
    entr = pkgs.entr;
    build = build;
    watch = watch;
  };
})
