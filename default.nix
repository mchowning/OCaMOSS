with import <nixpkgs> {};

let

 shared = import ./shared.nix;

in

shared.ocamlPackages.buildDunePackage {
  pname = "OCaMoss";
  version = "0.1";
  useDune2 = true;
  src = nix-gitignore.gitignoreFilterSource lib.cleanSourceFilter [] ./.;
  doCheck = true;
  buildInputs = shared.buildInputs;
}
