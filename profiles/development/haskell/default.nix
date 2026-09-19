{ pkgs
, primaryUser
, haskellPackages ? pkgs.haskellPackages
, extraHaskellPackages ? (_:[])
, ...}:
let
  haskellEnv = haskellPackages.ghcWithPackages(hsPkgs:
    let
      buildTools =
        with hsPkgs;
        [ cabal-install
          cabal2nix ];
      devTools =
        with hsPkgs;
        [ hoogle
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
  };
}
