{pkgs, utils, ...}:
utils.env.packagesEnvironment (with pkgs;
  [ prismlauncher
    # Open source games
    lbreakouthd
    kbounce
    bomber
    kbounce
    chiaki
  ])
