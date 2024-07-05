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
      nixosConfigurations =
        let
          mkSystem = { systemArch, systemConfigPath, homeManagerPath }:
            nixpkgs.lib.nixosSystem {
              system = systemArch;
              specialArgs = {
                inherit inputs;
                pkgs = import nixpkgs {
                  system = systemArch;
                  config.allowUnfree = true;
                  config.cudaSupport = true;
                };
              };
              modules = [
                (import systemConfigPath { inherit inputs; })
                home-manager.nixosModules.home-manager {
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users.rebecca = homeManagerPath;
                  home-manager.extraSpecialArgs = { inherit inputs; system = systemArch; };
                }
              ];
            };
        in
          {
            "fillory" = mkSystem {
              systemArch = "x86_64-linux";
              systemConfigPath = ./nixos-configurations/fillory/configuration.nix;
              homeManagerPath = ./fillory.nix;
            };

            "julia" = mkSystem {
              systemArch = "x86_64-linux";
              systemConfigPath = ./nixos-configurations/julia/configuration.nix;
              homeManagerPath = ./julia.nix;
            };
          };
    };
}
