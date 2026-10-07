# Development harness for the xmonad config: a cabal package wrapping
# config/ so it can be loaded in ghci and unit tested. Deployment still goes
# through services.xserver.windowManager.xmonad in default.nix; this only
# exists for `nix develop .#xmonad` and `nix flake check`.
{ pkgs }:
let
  inherit (pkgs) lib;
  # Match the NixOS module's default so the dev shell sees the same GHC and
  # xmonad versions that get deployed.
  haskellPackages = pkgs.haskellPackages;
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./xmonad-config.cabal
      ./config
      ./test
    ];
  };
  package = haskellPackages.callCabal2nix "xmonad-config" src { };
in
{
  inherit package;
  shell = haskellPackages.shellFor {
    packages = _: [ package ];
    withHoogle = false;
    nativeBuildInputs = [
      haskellPackages.cabal-install
      haskellPackages.ghcid
    ];
  };
}
