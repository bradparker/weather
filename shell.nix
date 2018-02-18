{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  direnv = nixpkgs.direnv;

  haskellPackages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.pkgs.haskell.packages.${compiler};
  env = (import ./default.nix { inherit nixpkgs compiler; }).env;
  hlint = haskellPackages.hlint;
  cabal = haskellPackages.cabal-install;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs = drv.nativeBuildInputs ++ [ hlint cabal direnv ];
  })
