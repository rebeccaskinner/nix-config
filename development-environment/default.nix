{ pkgs
, utils
, haskell-formatter-package ? ./haskell/formatter/stylish-haskell.nix
, ...
}:
utils.env.concatEnvironments [
  (import ./haskell { inherit pkgs utils; formatter = haskell-formatter-package;})
  (import ./rust {inherit pkgs utils;})
  (import ./gcc {inherit pkgs utils;})
  (import ./global-dev-env {inherit pkgs utils;})
  (import ./nixdev {inherit pkgs utils;})
]
