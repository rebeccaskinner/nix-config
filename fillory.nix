{ config, pkgs, pkgsStable, cudaPkgs, inputs, system, ... }:

let

  utils = import ./utils;
  load = f: import f { inherit config pkgs pkgsStable cudaPkgs inputs system utils; };

  cfg = p: utils.env.configOnlyEnvironment (import p);
  mkConfigs = cfgPaths:
    utils.env.concatEnvironments (builtins.map cfg cfgPaths);

  mkImport = p: utils.env.importOnlyEnvironment (
    import p { inherit config pkgs pkgsStable cudaPkgs inputs system utils; }
  );
  mkImports = importPaths:
    utils.env.concatEnvironments (builtins.map mkImport importPaths);

  desktopEnv = utils.env.concatEnvironments [
    (load ./desktop-environment/xserverTools.nix)
    (load ./desktop-environment/xmonad)
  ];

  gtkTheme = "Adwaita:dark";

  basicPackages = utils.env.packagesEnvironment (with pkgs; [
    bat
    pulsemixer
    file
    alsa-utils
    gifsicle
    dnsutils
    bitwarden-cli
    ripgrep
    unzip
    # renameutils for qmv, but it conflicts with imv the image viewer
    # renameutils
    rename
    graphicsmagick
    mat2
  ]);

  audioFilteringPackages = utils.env.packagesEnvironment (with pkgs; [
    easyeffects
    helvum
    pamixer
  ]);

  games = utils.env.packagesEnvironment (with pkgs; [
    nethack
    bastet
    nsnake
    ninvaders
    prismlauncher
    lbreakouthd
    kdePackages.bomber
    kdePackages.kbounce
    kdePackages.kolf
    kdePackages.kbreakout
    kdePackages.kollision
    kdePackages.ksnakeduel
    kdePackages.kreversi
    chiaki
    neverball
    xmoto
  ]);

  libbluray = pkgs.libbluray.override {
    withAACS = true;
    withBDplus = true;
    withJava = true;
  };
  vlc = pkgs.vlc.override { inherit libbluray; };
  whisper-cpp = cudaPkgs.callPackage ./collections/whisper-cpp { nvidia_x11 = cudaPkgs.linuxPackages.nvidia_x11; gcc = cudaPkgs.gcc13; };

  multimedia = let
    customPkgs = [ vlc
                   libbluray
                   # whisper-cpp
                   pkgsStable.ccextractor
                 ];
    defaultPkgs = (with pkgs; [
      makemkv
      mkvtoolnix
      handbrake
      ffmpeg
      jellyfin-media-player
      yt-dlp
      cdparanoiaIII
      abcde
    ]) ++ (with cudaPkgs; [blender]);
  in utils.env.packagesEnvironment (customPkgs ++ defaultPkgs);

  ebookTools = utils.env.packagesEnvironment (with pkgs; [
    scantailor-advanced
    tesseract
    pdftk
    ghostscript
    calibre
  ]);

  aspellPkgs = pkgs.aspellWithDicts(dicts: with dicts; [ en en-computers en-science ]);

  applications = utils.env.packagesEnvironment (with pkgs; [
    anki # flashcards
    baobab # disk usage visualization
    wireshark # network traffic
    gimp # image editing
    drawio # diagrams
    inkscape # svg editor
    scrot # screenshots
    qiv # image viewer
    bitwarden-desktop # password manager
    slack # communications
    element-desktop # matrix client
    thunderbird # email
    libreoffice # office suite
    signal-desktop # messaging
    kiwix # offline website archive
    kiwix-tools # tools for kiwix
    # simplex-chat-desktop # messaging
    kazam # screen recording
    aspellPkgs # spell checking
    pandoc # document conversion
    ispell # spell checking
    texlive.combined.scheme-full # latex
    python3Packages.pygments # syntax highlighting
    evince # document viewer
    kdePackages.okular # document viewer
    kdePackages.k3b # audio cd ripping and burning
  ]);

  devPackages = utils.env.packagesEnvironment (with pkgs; [
    shellcheck
    nix-index
    curl
    httpie
    jq
    s3cmd
  ]);

  rofi = import ./configs/rofi
    { rofi-hoogle-plugin = inputs.rofi-hoogle.outputs.packages.${system}.rofi-hoogle;
      inherit pkgs utils;
    };

  configs = mkImports [
    ./configs/kitty.nix
    ./configs/dircolors.nix
    ./configs/direnv.nix
    ./configs/git.nix
    ./configs/gpg.nix
    ./configs/fzf.nix
    ./configs/tmux.nix
    ./configs/bash.nix
    ./configs/java.nix
    ./configs/nextcloud-client.nix
    ./configs/chromium.nix
    ./configs/imv.nix
  ];

  haskellDevelopmentEnv = import ./development-environment/haskell {
    inherit pkgs utils;
    formatter = ./development-environment/haskell/formatter/fourmolu.nix;
    haskellVersion = 98;
  };

  rustDevelopmentEnv =
    import ./development-environment/rust { inherit pkgs utils; };

  gccDevelopmentEnv =
    import ./development-environment/gcc { inherit pkgs utils; };

  globalDevelopmentEnv =
    import ./development-environment/global-dev-env { inherit pkgs utils; };

  nvimConfig = import ./development-environment/nvim { inherit pkgs utils; };

  devTools = utils.env.concatEnvironments [
    devPackages
    haskellDevelopmentEnv
    rustDevelopmentEnv
    gccDevelopmentEnv
    globalDevelopmentEnv
    nvimConfig
  ];

  emacsConfig = import ./emacs {
    inherit pkgs utils;
    createMacosSymlink = false;
    emacsPackage = pkgs.emacs;
    extraPackages = ePkgs:
      (with ePkgs; [ rustic cargo hasklig-mode haskell-mode nix-haskell-mode ]);
    extraConfigs =
      [ (builtins.readFile ./development-environment/rust/rust.el) ];
  };

  environment =
    utils.env.concatEnvironments [
      desktopEnv
      basicPackages
      configs
      games
      multimedia
      ebookTools
      applications
      rofi
      devTools
      emacsConfig
      audioFilteringPackages
    ];
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rebecca";
  home.homeDirectory = "/home/rebecca";
  imports = environment.imports;
  home.packages = environment.packages;
  home.sessionVariables = { GTK_THEME = gtkTheme; };
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";

}
