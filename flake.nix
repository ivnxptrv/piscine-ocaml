{
  description = "OCaml development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        # Use the latest stable OCaml version available in nixpkgs
        ocamlPkgs = pkgs.ocamlPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            # The Compiler and Build System
            ocamlPkgs.ocaml
            ocamlPkgs.dune_3
            ocamlPkgs.findlib

            # Tooling & REPL
            ocamlPkgs.utop             # Advanced REPL
            ocamlPkgs.ocaml-lsp        # Language Server
            ocamlPkgs.ocamlformat      # Formatter
            # pkgs.ocaml-ng.ocamlPackages_5_1.ocaml-top # Graphical REPL (optional)

            # Extra utilities
            pkgs.opam                  # For managing external packages if needed
          ];

          shellHook = ''
            echo "OCaml Development Environment Loaded"
            echo "Compiler: $(ocaml -version)"
            echo "Build System: dune"
            echo "REPL: utop"
          '';
        };
      });
}
