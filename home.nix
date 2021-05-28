{ config, pkgs, ... }:

let
  gtkDarkTheme = { gtk-application-prefer-dark-theme = true; };

  haskellDevEnvironment = pkgs.haskellPackages.ghcWithPackages(hsPkgs: with hsPkgs; [
    ## Haskell Tooling
    cabal-install
    cabal2nix
    stylish-haskell
    ghcide
    hoogle
    threadscope

    ## Common Haskell Libraries
    stm
    bytestring
    text
    containers
    vector
    time
    unix
    mtl
    transformers
    aeson
    aeson-pretty
    lens
    conduit
  ]);

  commandLineTools = with pkgs; [
    htop
    bat
    silver-searcher
    pulsemixer
    curl
    jq
    httpie
    tmux
    file
  ];

  polybarFonts = with pkgs; [
    font-awesome-ttf
    siji
    material-design-icons
  ];

  emacsFonts = with pkgs; [
    hasklig
  ];

  writingTools = with pkgs; [
    pandoc
    evince
  ];

  xserverTools = with pkgs; [
    xmobar
    scrot
    trayer
    neofetch
    qiv
    pcmanfm
    networkmanagerapplet
    xmonad-log
  ];

  productivity = with pkgs; [
    slack
    thunderbird
    gimp
    drawio
  ];

  wallpapers = with pkgs.nixos-artwork.wallpapers; [
    dracula
    gnome-dark
    mosaic-blue
    nineish
    nineish-dark-gray
    simple-dark-gray
  ];

  games = with pkgs; [
    dwarf-fortress
    nethack
  ];

  nixTools = with pkgs; [
    nix-prefetch-scripts
  ];

in
{
  nixpkgs.config.allowUnfree = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rebecca";
  home.homeDirectory = "/home/rebecca";

  imports = (import ./programs)
         ++ (import ./services);

  home.packages =
    [haskellDevEnvironment]
    ++ commandLineTools
    ++ writingTools
    ++ xserverTools
    ++ polybarFonts
    ++ emacsFonts
    ++ wallpapers
    ++ games
    ++ nixTools
    ++ productivity;

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
    gtk3.extraConfig = gtkDarkTheme;
  };

  home.file = {
    ".config/gtk-4.0/settings.ini" = {
      source = ./gtk-4.0-settings.ini;
      recursive = false;
    };
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "org.gnome.Evince.desktop";
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
