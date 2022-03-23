{utils, pkgs, ...}:
utils.env.packagesEnvironment (with pkgs;
  [ siji
    material-design-icons
    hasklig
    font-awesome
    symbola
  ])
