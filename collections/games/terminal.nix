{pkgs, utils, ...}:
utils.env.packagesEnvironment (with pkgs;
  [ nethack
    bastet
    nsnake
    ninvaders
  ])
