{ pkgs
, utils
, ...
}:
let
  extensions = with pkgs.gnomeExtensions; [
    status-icons
    appindicator
    removable-drive-menu
    blur-my-shell
  ];
  gnome-packages = with pkgs; [
    gnome-tweaks
    vanilla-dmz
  ];
in utils.env.packagesEnvironment (extensions ++ gnome-packages)
