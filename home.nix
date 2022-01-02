{ config, pkgs, ... }:

let

  # should be one of "kde" or "xmonad";
  desktopEnvironment = "kde";
  # desktopEnvironment = "xmonad";

  dungeondraft = import ./programs/dungeondraft { inherit pkgs; };

  gtkDarkTheme =
    { gtk-application-prefer-dark-theme = true;
    };

  utils = import ./homeutils.nix { inherit config pkgs; };

  systemTools = with pkgs; [
    baobab # Disk usage tool
    bitwarden
    wireshark
    kazam
    qiv
  ];

  productivity = with pkgs; [
    slack
    thunderbird
  ];

  games = with pkgs; [
    dwarf-fortress
    nethack
    minecraft
    dungeondraft
  ];

  nixTools = with pkgs; [
    nix-prefetch-scripts
    rnix-lsp
  ];

  multimedia = with pkgs; [
    vlc
  ];

  devEnv = import ./collections/development-environment.nix { inherit utils config pkgs; };

  desktopEnv = import ./desktop-environment/config.nix
    { inherit desktopEnvironment utils; };

  writingEnv = import ./collections/writing-tools.nix
    { inherit utils config pkgs;
      includeLatex = true;
      includeGraphicalTools = true;
    };

  commandLineEnv = import ./collections/command-line-env.nix { inherit utils config pkgs; };
in
{
  nixpkgs.overlays = [ ];
  nixpkgs.config.allowUnfree = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rebecca";
  home.homeDirectory = "/home/rebecca";

  imports = (import ./programs)
            ++ (import ./services)
            ++ (import ./collections/fonts)
            ++ (import ./collections/graphics)
            ++ devEnv
            ++ commandLineEnv
            ++ writingEnv
            ++ desktopEnv;

  home.packages =
    systemTools
    ++ games
    ++ nixTools
    ++ productivity
    ++ multimedia;

  home.keyboard.options = ["ctrl:nocaps"];

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };

  services.emacs.enable = true;
  services.emacs.client.enable = true;
  services.blueman-applet.enable = true;
  services.network-manager-applet.enable = true;

  gtk = {
    enable = true;
    gtk4.extraConfig = gtkDarkTheme;
    gtk3.extraConfig = gtkDarkTheme;
    iconTheme = {
      package = pkgs.beauty-line-icon-theme;
      name = "elementary";
    };
  };


  xdg = {
    configFile = {
      "fourmolu.yaml" = {
        source = ./fourmolu.yaml;
        recursive = false;
      };
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
