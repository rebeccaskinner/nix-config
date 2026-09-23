{ pkgs
, utils
, extraLibs ? []
, haskellVersion ? (haskell: haskell.packages.ghc912)
, ...
}:

let

  # ghc912Overrides = import ./ghc912.nix {
  #   haskellPackages = pkgs.haskell.packages;
  #   overrideCabal = pkgs.haskell.lib.overrideCabal;
  #   fetchFromGitHub = pkgs.fetchFromGitHub;
  # };

  haskell = haskellVersion (pkgs.lib.recursiveUpdate pkgs.haskell.packages {
    # ghc912 = pkgs.haskell.packages.ghc912.override ghc912Overrides;
  });

  ghciConfig = import ./settings/ghci;

  devPackages = haskell.ghcWithPackages(hsPkgs:
    let
      buildTools =
        with hsPkgs;
        [ cabal-install
          cabal2nix ];
      devTools =
        with hsPkgs;
        [ # hoogle
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
          deepseq
          stm
          aeson ];
    in builtins.concatLists [ buildTools devTools basicLibraries extraLibs]
  );

in
{ packages = [ devPackages ];
  imports = [ ghciConfig ];
  emacsExtraPackages = epkgs:
    with epkgs; [
      hasklig-mode
      haskell-mode
      nix-haskell-mode
    ];
}
