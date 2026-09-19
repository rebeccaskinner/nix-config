{ pkgs
, primaryUser
, ...
}:
let
  gnome-extensions = with pkgs.gnomeExtensions; [
    status-icons
    appindicator
    removable-drive-menu
    blur-my-shell
  ];
  gnome-packages = with pkgs; [
    gnome-tweaks
    vanilla-dmz
  ];
in
{
  services = {
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };

  users.users.${primaryUser}.packages =
    gnome-extensions ++ gnome-packages;
}
