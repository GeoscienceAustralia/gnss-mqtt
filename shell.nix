{
  pkgs ? import ./nix {},
}:

let
  projectName = "gnss-mqtt";

  buildTools = with pkgs; [
    aws-iam-authenticator
    google-cloud-sdk
    kubectl
    mosquitto
    go_1_12
    erlang
    rebar3
    elixir
  ];

  env = pkgs.buildEnv {
    name = projectName + "-env";
    paths = buildTools;
  };

in
  pkgs.mkShell {
    buildInputs = [
      env
    ];
    shellHook = ''
      export PROJECT_NAME=${projectName}
    '';
  }
