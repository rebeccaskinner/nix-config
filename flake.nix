{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-24.11";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    rofi-hoogle.url = "github:rebeccaskinner/rofi-hoogle/main";
    rofi-hoogle.inputs.nixpkgs.follows = "nixpkgs";
    foundryvtt.url = "github:reckenrode/nix-foundryvtt";
    foundryvtt.inputs.nixpkgs.follows = "nixpkgs";
    darkplum-theme.url = "github:rebeccaskinner/darkplum-theme";
    darkplum-theme.flake = false;

    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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
    , foundryvtt
    , llm-agents
    , ... }@inputs:
    let
      xmonadDev = import ./profiles/desktop-environment/xmonad/dev.nix {
        pkgs = import nixpkgs { system = "x86_64-linux"; };
      };
    in
    {
      devShells.x86_64-linux.xmonad = xmonadDev.shell;
      checks.x86_64-linux.xmonad-config = xmonadDev.package;

      darwinConfigurations = {
        gimli = import ./hosts/gimli inputs;
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
            pkgsStable = import nixpkgs-stable {
              system = "x86_64-linux";
              config.allowUnfree = true;
              config.cudaSupport = false;
            };
            system = "x86_64-linux";
          };
          modules = [
            (import ./nixos-configurations/daystrom/configuration.nix { inherit inputs; })
            foundryvtt.nixosModules.foundryvtt
          ];
        };

        fillory = import ./hosts/fillory inputs;
        julia = import ./hosts/julia inputs;
      };
    };
}
