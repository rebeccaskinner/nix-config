{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-24.11";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    rofi-hoogle.url = "github:rebeccaskinner/rofi-hoogle/main";
    rofi-hoogle.inputs.nixpkgs.follows = "nixpkgs";

    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-stable
    , rofi-hoogle
    , home-manager
    , darwin
    , ... }@inputs:
    {
      darwinConfigurations = {
        "gimli" = darwin.lib.darwinSystem {
          # username = "rebeccaskinner";
          # hostname = "gimli";
          system = "aarch64-darwin";
          specialArgs = {
            inherit inputs;
            pkgs = import nixpkgs {
              system = "aarch64-darwin";
              config.allowUnfree = true;
            };
          };

          modules = [
            (import ./nix-darwin-configuration/gimli/configuration.nix { inherit inputs; })

            home-manager.darwinModules.home-manager {
              users.users.rebeccaskinner.home = "/Users/rebeccaskinner";
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.rebeccaskinner = ./gimli.nix;
              home-manager.extraSpecialArgs = {
                inherit inputs;
                system = "aarch64-darwin";
              };
            }

          ];
        };
      };

      nixosConfigurations = {
        "daystrom" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {
            inherit inputs;
            pkgs = import nixpkgs {
              system = "x86_64-linux";
              config.allowUnfree = true;
              config.cudaSupport = false;
            };
          };
          modules = [
            (import ./nixos-configurations/daystrom/configuration.nix { inherit inputs; })

          ];
        };

        "fillory" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {
            inherit inputs;
            pkgs = import nixpkgs {
              system = "x86_64-linux";
              config.allowUnfree = true;
              config.cudaSupport = false;
            };
          };

          modules = [
            (import ./nixos-configurations/fillory/configuration.nix { inherit inputs; })
            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.rebecca = ./fillory.nix;
              home-manager.extraSpecialArgs = {
                inherit inputs;
                cudaPkgs = import nixpkgs {
                  system = "x86_64-linux";
                  config.allowUnfree = true;
                  config.cudaSupport = true;
                };
                pkgsStable = import nixpkgs-stable {
                  system = "x86_64-linux";
                  config.allowUnfree = true;
                  config.cudaSupport = false;
                };
                system = "x86_64-linux";
              };
            }
          ];
        };

        "julia" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {
            inherit inputs;
            pkgs = import nixpkgs {
              system = "x86_64-linux";
              config.allowUnfree = true;
              config.cudaSupport = false;
            };
            pkgsStable = import nixpkgs-stable {
              system = "x86_64-linux";
              config.allowUnfree = true;
              config.cudaSupport = false;
            };
          };

          modules = [
            (import ./nixos-configurations/julia/configuration.nix { inherit inputs; })
            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.rebecca = ./julia.nix;
              home-manager.extraSpecialArgs = { inherit inputs; system = "x86_64-linux"; };
            }
          ];
        };

      };
    };
}
