{ config, pkgs, ... }:

let

  fourmoluOverlay = import ./overlays/fourmolu;
  dungeondraft = import ./programs/dungeondraft { inherit pkgs; };

  gtkDarkTheme = { gtk-application-prefer-dark-theme = true; };
  aspellPkgs = pkgs.aspellWithDicts(dicts: with dicts; [ en en-computers en-science ]);

  haskellDevEnvironment = pkgs.haskellPackages.ghcWithPackages(hsPkgs: with hsPkgs; [
    ## Haskell Tooling
    cabal-install
    cabal2nix
    fourmolu
    ghcide
    hoogle
    threadscope
    stylish-haskell
    haskell-language-server
    hasktags

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

  rustPackages = with pkgs; [
    rustc
    rustfmt
    rust-analyzer
    cargo
    cargo-edit
  ];

  gccDevelopmentEnvironment = with pkgs; [
    gcc
    gnumake
    valgrind
    binutils
    elfutils
    gdb
    ctags
    ccls
  ];

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
    aspellPkgs
    s3cmd
    alsa-utils
    gifsicle
    ffmpeg
    gitg
  ];

  fonts = with pkgs; [
    font-awesome-ttf
    siji
    material-design-icons
    hasklig
    font-awesome
    symbola
  ];

  writingTools = with pkgs; [
    pandoc
    evince
    okular
    ispell
    texlive.combined.scheme-full
    pythonPackages.pygments
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
    xorg.xcursorthemes
    hicolor-icon-theme
    kazam # screen recording tool
  ];

  systemTools = with pkgs; [
    baobab # Disk usage tool
    gotop  # command line system monitor
    dnsutils
    bitwarden
    bitwarden-cli
    wireshark
  ];

  productivity = with pkgs; [
    slack
    thunderbird
    gimp
    krita
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

in
{
  nixpkgs.overlays = [ fourmoluOverlay ];
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
    ++ rustPackages
    ++ gccDevelopmentEnvironment
    ++ commandLineTools
    ++ writingTools
    ++ xserverTools
    ++ systemTools
    ++ fonts
    ++ wallpapers
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
    gtk3.extraConfig = gtkDarkTheme;
    iconTheme = {
      package = pkgs.beauty-line-icon-theme;
      name = "elementary";
    };
  };

  home.file = {
    ".config/gtk-4.0/settings.ini" = {
      source = ./gtk-4.0-settings.ini;
      recursive = false;
    };
  };

  xdg = {
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = "org.gnome.Evince.desktop";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/chrome" = "firefox.desktop";
        "text/html" = "firefox.desktop";
        "application/x-extension-htm" = "firefox.desktop";
        "application/x-extension-html" = "firefox.desktop";
        "application/x-extension-shtml" = "firefox.desktop";
        "application/xhtml+xml" = "firefox.desktop";
        "application/x-extension-xhtml" = "firefox.desktop";
        "application/x-extension-xht" = "firefox.desktop";
      };
    };

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
