{ nixpkgs
, nixpkgs-stable
, home-manager
, ... }@inputs:
let
  system = "x86_64-linux";
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    config.cudaSupport = false;
  };
  pkgsStable = import nixpkgs-stable {
    inherit system;
    config.allowUnfree = true;
    config.cudaSupport = false;
  };
  haskellPackages = pkgs.haskell.packages.ghc914;
in 
nixpkgs.lib.nixosSystem {
  specialArgs = {
    inherit inputs pkgs pkgsStable system haskellPackages;
  };

  modules = [
    ./configuration.nix

    home-manager.nixosModules.home-manager {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.rebecca = ./julia.nix;
      home-manager.extraSpecialArgs = {
        inherit pkgs pkgsStable system;
      };
    }
  ];
}
