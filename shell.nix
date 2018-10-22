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
    kops
    kubectl
    hlint
    mosquitto
    ghc
    (python36.buildEnv.override {
      ignoreCollisions = true;
      extraLibs = with python36Packages; [
        boto3
        requests
        paho-mqtt
        requests-aws4auth
      ];
    })
  ];
}
