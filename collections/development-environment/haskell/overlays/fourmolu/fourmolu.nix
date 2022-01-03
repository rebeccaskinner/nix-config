{ pkgs ? import <nixpkgs> {} }:
let
  fourmoluSrc = pkgs.haskellPackages.haskellSrc2nix {
    name = "fourmolu";
    src = builtins.fetchTarball {
      name = "fourmolu-patch";
      url = "https://github.com/rebeccaskinner/fourmolu/archive/924224a.tar.gz";
      sha256 = "0f3xafbaxbhpsrh69a5rcf247ya9j895k3591lmk2f2ph8sl350m";
    };
  };
in pkgs.haskellPackages.callPackage "${fourmoluSrc}/default.nix" {}
