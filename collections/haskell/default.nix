{ config
, pkgs
, extraLibs ? []
, ...}:

let
  haskellDevEnvironment = pkgs.haskellPackages.ghcWithPackages(hsPkgs: with hsPkgs;
    let
      buildTools = [ cabal-install
                     cabal2nix
                   ];
      devTools = [ stylish-haskell
                   hoogle
                   threadscope
                   hasktags
                   haskell-language-server
                 ];

      basicLibraries = [ bytestring
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
                         aeson
                       ];

    in builtins.concatLists [buildTools devTools basicLibraries extraLibs]
  );

in haskellDevEnvironment
