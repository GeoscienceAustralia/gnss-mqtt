{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
pkgs.stdenv.mkDerivation {
  name = "testing";
  buildInputs = with pkgs; [
    google-cloud-sdk
    kubectl
    mosquitto
    go
  ];
}
