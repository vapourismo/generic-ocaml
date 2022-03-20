{
  description = "generic-ocaml";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    flake-utils.url = github:numtide/flake-utils;
    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat }: flake-utils.lib.eachDefaultSystem (system:
    with import nixpkgs { inherit system; };

    let
      ocamlPackages = ocaml-ng.ocamlPackages_4_13;
    in
    {
      defaultPackage = ocamlPackages.buildDunePackage {
        pname = "ppx_deriving";
        version = "0.0.0";

        useDune2 = true;

        src = self;

        buildInputs = with ocamlPackages; [
          ppxlib
        ];

        doCheck = true;

        checkInputs = with ocamlPackages; [
          alcotest
        ];
      };

      devShell = mkShell {
        nativeBuildInputs = with ocamlPackages; [
          ocaml
          ocaml-lsp
          ocamlformat
          findlib
          dune_2
          utop
          nixpkgs-fmt
          inotify-tools
          odoc
        ];

        buildInputs =
          self.defaultPackage.${system}.buildInputs
          ++ self.defaultPackage.${system}.nativeBuildInputs;
      };
    }
  );
}
