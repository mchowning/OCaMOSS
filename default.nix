with import <nixpkgs> {};

let
 ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_13;
in

ocamlPackages.buildDunePackage {
  pname = "OCaMoss";
  version = "0.1";
  useDune2 = true;
  src = nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [] ./.;
  doCheck = true;
  buildInputs = with ocamlPackages; [
     ansiterminal
     core
     core_extended
     ounit
     yojson
  ];
}
