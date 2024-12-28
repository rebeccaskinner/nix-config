{ inputs }:
{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  nix.settings.experimental-features = ["nix-command" "flakes"];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [ 
    wget
    vim
    git
    man-pages
    man-pages-posix
    scowl
    ripgrep
    bottom
  ];

  # Use custom location for configuration.nix.
  environment.darwinConfig = "$HOME/projects/nix-config/nix-darwin-configuration/gimli/configuration.nix";

  # Enable alternative shell support in nix-darwin.
  # programs.fish.enable = true;
  programs.bash.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
}

