{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    rofi-hoogle.url = "github:rebeccaskinner/rofi-hoogle/main";
    rofi-hoogle.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, rofi-hoogle, home-manager, ... }@inputs:
  {
    nixosConfigurations = {
      "fillory" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit inputs;
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
            # config.cudaSupport = true;
          };
        };

        modules = [
          (import ./nixos-configurations/fillory/configuration.nix { inherit inputs; })
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.rebecca = ./fillory.nix;
            home-manager.extraSpecialArgs = { inherit inputs; system = "x86_64-linux"; };
          }
        ];
      };
    };
  };
}
