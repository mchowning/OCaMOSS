let

 pkgs = import <nixpkgs> {};

 # choose the ocaml version you want to use
 ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_13;

in

{
  inherit ocamlPackages;
  buildInputs = with ocamlPackages; [
     ansiterminal
     core
     core_extended
     ounit
     yojson
  ];
}
