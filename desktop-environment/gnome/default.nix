{ pkgs
, utils
, ...
}:
let
  extensions = with pkgs.gnomeExtensions; [
    tray-icons-reloaded
    removable-drive-menu
  ];
  gnome-packages = with pkgs; [ vanilla-dmz ];
in utils.env.packagesEnvironment (extensions ++ gnome-packages)
