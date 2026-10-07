{ nixpkgs
, nixpkgs-stable
, home-manager
, foundryvtt
, ... }@inputs:
let
  system = "x86_64-linux";
  primaryUser = "rebecca";
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
  profiles = import ../../profiles;
in
nixpkgs.lib.nixosSystem {
  specialArgs = {
    inherit inputs pkgsStable system primaryUser;
  };

  modules = [
    nixpkgs.nixosModules.readOnlyPkgs
    { nixpkgs.pkgs = pkgs; }

    ./configuration.nix
    foundryvtt.nixosModules.foundryvtt

    home-manager.nixosModules.home-manager
    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.backupFileExtension = "hm-bak";
      home-manager.users.${primaryUser} = ./daystrom.nix;
      home-manager.extraSpecialArgs = {
        inherit primaryUser pkgs pkgsStable system;
      };
    }

    profiles.cli

    ({...}@args:
      import profiles.development.cli (args // {
        pkgs = pkgsStable;
      })
    )

    profiles.emacs
    { profiles.emacs.package = pkgs.emacs-nox; }
  ];
}
