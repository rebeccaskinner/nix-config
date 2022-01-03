{utils, pkgs, ...}:

let
  devDeps = [./haskell
             ./rust
             ./gcc
             ./tools.nix ];
in utils.callCollections devDeps {inherit pkgs;}
