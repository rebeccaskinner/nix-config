{ nixpkgs
, nixpkgs-stable
, home-manager
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
  haskellPackages = pkgs.haskell.packages.ghc912;
  profiles = import ../../profiles;
in
nixpkgs.lib.nixosSystem {
  specialArgs = {
    inherit inputs pkgsStable system haskellPackages primaryUser;
  };

  modules = [
    nixpkgs.nixosModules.readOnlyPkgs
    { nixpkgs.pkgs = pkgs; }

    ./configuration.nix

    home-manager.nixosModules.home-manager
    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.${primaryUser} = ./julia.nix;
      home-manager.extraSpecialArgs = {
        inherit pkgs pkgsStable system;
      };
    }

    profiles.cli
    profiles.desktop-environment.gnome
    profiles.general-desktop

    profiles.development.cli
    profiles.development.dev-tools
    profiles.development.gcc
    profiles.development.haskell
    profiles.development.nix
    # profiles.development.rust

    profiles.emacs

    profiles.multimedia.client
    # profiles.multimedia.encoding
    # profiles.multimedia.ripping

    # profiles.latex
    profiles.office
    # profiles.writing

    profiles.games.steam
    profiles.games.open-source
  ];
}
