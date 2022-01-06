{ pkgs
, utils
, formatter ? ./formatter/stylish-haskell.nix
, extraLibs ? []
, ...}:

let
  fmt =
    import formatter { haskellPackages = pkgs.haskellPackages; };

  haskellDevEnvironment = pkgs.haskellPackages.ghcWithPackages(hsPkgs:
    let
      buildTools =
        with hsPkgs;
        [ cabal-install
          cabal2nix ];
      devTools =
        with hsPkgs;
        [ fmt.package
          hoogle
          threadscope
          hasktags
          haskell-language-server ];
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
  emacsFormatterFunction = ''
(defun haskell-formatter-path ()
  "Return the path to the binary that should be called for format programs."
  "${fmt.exec}"
  );
'';
  emacsConfig = builtins.readFile ./emacs-haskell-config.el;
in
{ packages = [fmt.package haskellDevEnvironment];
  imports = [ fmt.config ];
  emacsExtraPackages = epkgs:
    with epkgs; [
      dante
      hasklig-mode
      lsp-haskell
    ];
  emacsExtraConfig =
    emacsFormatterFunction + emacsConfig;
}
