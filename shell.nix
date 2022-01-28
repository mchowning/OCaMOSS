let
 pkgs = import <nixpkgs> {};
 shared = import ./shared.nix;
in
pkgs.mkShell {
  # build tools
  nativeBuildInputs = with shared.ocamlPackages; [
     ocaml
     findlib
     dune_2
     # merlin
     ocaml-lsp
     utop
    ];
  # dependencies
  buildInputs = shared.buildInputs;
}
