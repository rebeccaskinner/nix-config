let
  func = import ./funcutils.nix;
in
rec {
  cons = x: xs: [x] ++ xs;
  reverse = builtins.foldl' (func.flip cons) [];
}
