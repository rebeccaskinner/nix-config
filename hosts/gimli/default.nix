{ nixpkgs
, home-manager
, darwin
, llm-agents
, ... }@inputs:
let
  system = "aarch64-darwin";
  primaryUser = "rebeccaskinner";
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
  haskellPackages = pkgs.haskell.packages.ghc912;
  profiles = import ../../profiles;
in
darwin.lib.darwinSystem {
  inherit system;
  specialArgs = {
    inherit inputs pkgs system haskellPackages primaryUser llm-agents;
  };

  modules = [
    ./configuration.nix

    home-manager.darwinModules.home-manager
    {
      system.primaryUser = primaryUser;
      users.users.${primaryUser}.home = "/Users/${primaryUser}";
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.${primaryUser} = ./gimli.nix;
      home-manager.extraSpecialArgs = {
        inherit primaryUser system;
      };
    }

    profiles.ai-assistants
    profiles.cli
    profiles.gnu

    profiles.development.haskell
    profiles.development.macos
    profiles.development.nix
    profiles.development.rust

    profiles.emacs
  ];
}
