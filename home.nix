{ config, pkgs, ... }:

let
  gtkDarkTheme = { gtk-application-prefer-dark-theme = true; };
in
{
  nixpkgs.config.allowUnfree = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rebecca";
  home.homeDirectory = "/home/rebecca";

  home.packages = with pkgs; [
    htop
    slack
    bat
    silver-searcher
    pandoc
    thunderbird
    ctags
    xmobar
    dmenu
    scrot
    trayer
    networkmanagerapplet
    neofetch
    qiv
    pcmanfm
  ];

  home.keyboard.options = ["ctrl:nocaps"];

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };

  services.emacs.enable = true;
  services.emacs.client.enable = true;

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.ace-window
      epkgs.flycheck
      epkgs.direnv
      epkgs.fill-column-indicator
      epkgs.dante
      epkgs.auto-complete
      epkgs.pdf-tools
      epkgs.dhall-mode
      epkgs.proof-general
      epkgs.rebecca-theme
      epkgs.nix-mode
      epkgs.magit
      epkgs.ox-gfm
      epkgs.nix-buffer
      epkgs.nix-sandbox
      epkgs.restclient
      epkgs.protobuf-mode
      epkgs.darcula-theme
      epkgs.format-sql
      epkgs.paredit
      epkgs.ox-pandoc
      epkgs.json-mode
      epkgs.graphviz-dot-mode
      epkgs.go-mode
      epkgs.go-playground
      epkgs.expand-region
      epkgs.cargo
      epkgs.rainbow-delimiters
      epkgs.sql-indent
      epkgs.spacemacs-theme
      epkgs.use-package
      epkgs.pdf-tools
    ];
  };

  programs.gpg = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userEmail = "rebecca@rebeccaskinner.net";
    userName = "rebecca skinner";
    aliases = {
      co = "checkout";
      br = "branch";
      ff = "merge --ff-only";
      st = "status";
    };
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
    };
  };

  programs.kitty = {
    enable = true;
    font.name = "Fira Code";
  };

  programs.dircolors = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableNixDirenvIntegration = true;
  };

  programs.bash = {
    enable = true;
    enableVteIntegration = true;
    shellAliases = {
      ll = "ls -alF";
      la = "ls -A";
      ls = "ls --color=tty --classify";
      gitk = "gitk --all &";
      qiv = "qiv -t -I";
      emacs = "emacsclient -nw";
    };
  };

  programs.feh = {
    enable = true;
  };

  gtk = {
    enable = true;
    gtk3.extraConfig = gtkDarkTheme;
  };

  home.file = {
    ".emacs.d" = {
      source = ./emacs;
      recursive = true;
    };
    ".xmobarrc" = {
      source = ./xmonad/xmobarrc;
      recursive = false;
    };
    ".config/gtk-4.0/settings.ini" = {
      source = ./gtk-4.0-settings.ini;
      recursive = false;
    };
    ".ghci" = {
      source = ./ghci;
      recursive = false;
    };
  };

  xsession = {
    enable = true;
    initExtra = ''
xrandr --dpi 90
xrandr --output eDP-1-1 --brightness 0.2
xrandr --output eDP-1-1 --off
feh --bg-scale /home/rebecca/.config/wallpaper
'';
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad/xmonad.hs;
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
