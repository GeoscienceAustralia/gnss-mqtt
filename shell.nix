{
  pkgs ? import ./nix {},
}:

let
  projectName = "gnss-mqtt";

  buildTools = with pkgs; [
    terraform
    aws-iam-authenticator
    google-cloud-sdk
    kubectl
    mosquitto
    go_1_12
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
