{ pkgs
, utils
, ...
}:
let
  extensions = with pkgs.gnomeExtensions; [
    tray-icons-reloaded
    removable-drive-menu
  ];
in utils.env.packagesEnvironment extensions
