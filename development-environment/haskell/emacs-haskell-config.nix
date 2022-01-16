{ pkgs, emacsPackages }:

emacsPackages.melpaBuild rec {
  name = "emacs-haskell-config";
  pname = "emacs-haskell-config";
  version = "0.0.1";
  src = ./emacs-haskell-config.el;
  recipe = pkgs.writeText "recipe" ''

'';
}
