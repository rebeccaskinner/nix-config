{ pkgs
, utils
, ...
}:
utils.env.packagesEnvironment(
  with pkgs; [
    xcursor-themes
    hicolor-icon-theme
    kdePackages.breeze-gtk
  ])
