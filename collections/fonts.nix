{utils, pkgs, ...}:
utils.env.packagesEnvironment (with pkgs;
  [ font-awesome-ttf
    siji
    material-design-icons
    hasklig
    font-awesome
    symbola
  ])
