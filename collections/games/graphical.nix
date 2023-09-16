{pkgs, utils, ...}:
utils.env.packagesEnvironment (with pkgs;
  [ prismlauncher
    minetest
    minecraft
    # Open source games
    lbreakouthd
    kbounce
    bomber
    crack_attack
    kbounce
  ])
