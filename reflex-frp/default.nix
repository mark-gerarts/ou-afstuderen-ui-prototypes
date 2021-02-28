# See https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst#building-with-nix
# We omitted common + backend since we're only interested in building a frontend
# application.
{ system ? builtins.currentSystem }:

(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {

  packages = {
    frontend = ./frontend;
  };

  shells = {
    ghcjs = ["frontend"];
  };
})
