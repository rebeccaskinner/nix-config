{pkgs, utils, ...}:
utils.env.packagesEnvironment (with pkgs;
  [ prismlauncher
    # Open source games
    lbreakouthd
    kdePackages.bomber
    kdePackages.kbounce
    chiaki
  ])
