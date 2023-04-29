{ pkgs
, utils
, haskell-formatter-package ? null
, haskellVersion ? null
, ...
}:
utils.env.concatEnvironments [
  (import ./haskell { inherit pkgs utils haskellVersion; formatter = haskell-formatter-package;})
  (import ./rust {inherit pkgs utils;})
  (import ./gcc {inherit pkgs utils;})
  (import ./global-dev-env {inherit pkgs utils;})
  (import ./nixdev {inherit pkgs utils;})
  (import ./vscode.nix {inherit pkgs utils;})
  (import ./nvim {inherit pkgs utils;})
]
