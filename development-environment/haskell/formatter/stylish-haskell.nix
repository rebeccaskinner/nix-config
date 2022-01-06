{haskellPackages}:
let
  stylishHaskell = haskellPackages.stylish-haskell;
in
{
  package = stylishHaskell;
  config = {...}: {};
  exec = "${stylishHaskell}/bin/stylish-haskell";
}
