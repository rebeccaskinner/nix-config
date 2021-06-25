fixedPoint: pkgs: {
  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellFixedPoint: haskellPkgs: {
      fourmolu = import ./fourmolu.nix { inherit pkgs; };
    };
  };
}
