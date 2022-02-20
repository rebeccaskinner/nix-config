{ pkgs
, utils
, extraLibs ? []
, haskellVersion ? null
, formatter ? null
, ...}:

let
  haskell = if builtins.isNull haskellVersion
            then pkgs.haskellPackages
            else
              let versionStr = builtins.toString haskellVersion;
              in pkgs.haskell.packages."ghc${versionStr}";


  ghciConfig = import ./settings/ghci;

  devPackages = haskell.ghcWithPackages(hsPkgs:
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
    in builtins.concatLists [buildTools devTools basicLibraries extraLibs]
  );

  formatterEnvironment =
    let
      formatterPackage = import formatter { haskellPackages = haskell; };
      emacsFormatter = ''
        (defun haskell-formatter-path ()
          "Return the path to the binary that should be called for format programs."
          "${formatterPackage.exec}"
        );
      '';
    in
      { packages = [ formatterPackage.package ];
        imports = [ formatterPackage.config ];
        emacsFormatterFunction = emacsFormatter;
        emacsExtraConfig = emacsFormatter;
      };

  devEnvironment =
    { packages = [ devPackages ];
      imports = [ ghciConfig ];
      emacsExtraPackages = epkgs:
        with epkgs; [
          hasklig-mode
          haskell-mode
          nix-haskell-mode
        ];
    };

  pkg =
    if builtins.isNull formatter
    then devEnvironment
    else utils.env.mergeEnvironments devEnvironment formatterEnvironment;

in pkg
