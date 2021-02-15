{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  start-server = writeScriptBin "start-server" ''
    php -S localhost:8080 server.php
  '';

in mkShell {

  nativeBuildInputs = [
    php
  ];

  buildInputs = [
    start-server
  ];
}
