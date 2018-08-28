{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    rtcm
    mqtt-hs
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "testing";
  buildInputs = with pkgs; [
    hlint
    mosquitto
    ghc
  ];
  shellHook = "";
}
