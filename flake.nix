{
  description = "A minimal library that implements a small subset of DynamoDB's Client API";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in rec {
    packages.${system}.default = pkgs.ocamlPackages.buildDunePackage {
      pname = "dynamodb";
      version = "0.1.0";
      src = ./.;

      duneVersion = "3";

      buildInputs = with pkgs.ocamlPackages; [
        cohttp
        cohttp-lwt-unix
        digestif
        lwt
        ppx_deriving
        ppx_deriving_yojson
        uri
        yojson
      ];

      nativeBuildInputs = with pkgs; [
        ocaml
        ocamlPackages.findlib
      ];
    };

    devShells.${system}.default = pkgs.mkShell {
      inputsFrom = [ packages.${system}.default ];

      nativeBuildInputs = with pkgs; [
        ocaml
        dune_3
        ocamlPackages.utop
        ocamlformat
        ocamlPackages.ocaml-lsp
      ];
    };
  };
}
