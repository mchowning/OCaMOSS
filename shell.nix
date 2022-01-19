let
 pkgs = import <nixpkgs> {};

 # choose the ocaml version you want to use
 ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_13;

in
pkgs.mkShell {
  # build tools
  nativeBuildInputs = with ocamlPackages; [
     ocaml
     findlib
     dune_2
     # merlin
     ocaml-lsp
    ];
  # dependencies
  buildInputs = with ocamlPackages; [
     ansiterminal
     core
     core_extended
     ounit
     yojson
  ];
}
