{ config, pkgs, inputs, system, ... }:

let
  load     = f: import f { inherit pkgs utils; };
  utils    = import ./utils;

  cfg = p: utils.env.configOnlyEnvironment (import p);
  mkConfigs = cfgPaths: utils.env.concatEnvironments (builtins.map cfg cfgPaths);

  mkImport = p: utils.env.importOnlyEnvironment (import p);
  mkImports = importPaths: utils.env.concatEnvironments (builtins.map mkImport importPaths);

  packages =  utils.env.packagesEnvironment (with pkgs; [
    # common system tools
    coreutils
    htop
    curl
    httpie
    bat
    file
    ripgrep
    renameutils
    rename
    pandoc
    # networking tools
    wireshark
    # development tools
    shellcheck
    jq
    nix-prefetch-scripts
    # multimedia tools
    ffmpeg
    mat2
    # terminal games
    nethack
  ]);

  kittyConfig = load ./configs/kitty.nix;
  tmuxConfig = load ./configs/tmux.nix;

  configs = mkConfigs [
      ./configs/bash.nix
      ./configs/dircolors.nix
      ./configs/direnv.nix
      ./configs/git.nix
      ./configs/gpg.nix
      ./configs/fzf.nix
  ];

  haskellDevelopmentEnv = import ./development-environment/haskell {
    inherit pkgs utils;
    formatter = ./development-environment/haskell/formatter/fourmolu.nix;
    haskellVersion = 98;
  };

  rustDevelopmentEnv = import ./development-environment/rust {
    inherit pkgs utils;
  };

  globalDevelopmentEnv = import ./development-environment/global-dev-env {
    inherit pkgs utils;
  };

  nvimConfig = import ./development-environment/nvim {
    inherit pkgs utils;
  };

  devTools = utils.env.concatEnvironments [
    haskellDevelopmentEnv
    rustDevelopmentEnv
    globalDevelopmentEnv
    nvimConfig
    tmuxConfig
  ];

  emacsConfig = import ./emacs {
     inherit pkgs utils;
     createMacosSymlink = true;
     emacsPackage = pkgs.emacs-macport;
     extraPackages = ePkgs: (with ePkgs; [
       rustic
       cargo
       hasklig-mode
       haskell-mode
       nix-haskell-mode
     ]);
     extraConfigs = [
       (builtins.readFile ./development-environment/rust/rust.el)
     ];
  };

  environment = utils.env.concatEnvironments
    [ packages
      configs
      devTools
      emacsConfig
    ];
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rebeccaskinner";
  home.homeDirectory = "/Users/rebeccaskinner";
  imports = environment.imports;
  home.packages = environment.packages;

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
