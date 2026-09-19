# Basic Haskell toolchain for hacking on small projects outside of a
# project-specific nix environment.
#
# Hosts can pick a compiler by passing `haskellPackages` through
# specialArgs, and add libraries with `extraHaskellPackages`. Defaults are
# handled with `or` rather than argument defaults because the module system
# looks every named argument up and errors when one is missing.
{ pkgs
, primaryUser
, ...}@args:
let
  haskellPackages = args.haskellPackages or pkgs.haskellPackages;
  extraHaskellPackages = args.extraHaskellPackages or (_: []);

  haskellEnv = haskellPackages.ghcWithPackages(hsPkgs:
    let
      buildTools =
        with hsPkgs;
        [ cabal-install
          cabal2nix ];
      devTools =
        with hsPkgs;
        [ fourmolu
          hoogle
          hasktags
          hlint
        ];
      basicLibraries =
        with hsPkgs;
        [ bytestring
          text
          vector
          time
          unix
          mtl
          transformers
          array
          deepseq
          filepath
          process
          primitive
          stm
          aeson ];
      extraLibs = extraHaskellPackages hsPkgs;
    in builtins.concatLists [buildTools devTools basicLibraries extraLibs]
  );
in
{
  users.users.${primaryUser}.packages = [ haskellEnv ];
  home-manager.users.${primaryUser} = {
    imports = [
      ../../../development-environment/haskell/settings/ghci/default.nix
    ];
    xdg.configFile."fourmolu.yaml".source =
      ../../../development-environment/haskell/formatter/fourmolu.yaml;
  };
}
