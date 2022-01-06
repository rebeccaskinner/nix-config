{ pkgs
, utils
, ...
}:
utils.env.packagesEnvironment(
  with pkgs; [
    xorg.xcursorthemes
    hicolor-icon-theme
    breeze-gtk
  ])
