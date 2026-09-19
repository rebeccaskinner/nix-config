{pkgs, primaryUser, ...}:
{
  services = {
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };
    displayManager.sddm.enable = true;
    gnome = {
      gnome-keyring.enable = true;
      gcr-ssh-agent.enable = false;
    };
    libinput.enable = true;
    udisks2.enable = true;
    xserver = {
      enable = true;
      xkb.layout = "us";
      xkb.options = "ctrl:nocaps";
      windowManager.xmonad.enable = true;
    };
  };
  programs.dconf.enable = true;
  users.users.${primaryUser}.packages = with pkgs; [
      candy-icons
      hicolor-icon-theme
      kdePackages.breeze-gtk
      packmanfm
      thunar
      tumbler
      xcursor-themes
    ];
  home-manager.users.${primaryUser} = {
    imports = [
      ../desktop-environment/xmonad/blueman.nix
      ../desktop-environment/xmonad/dunst.nix
      ../desktop-environment/xmonad/feh.nix
      ../desktop-environment/xmonad/mimeApps.nix
      ../desktop-environment/xmonad/picom.nix
      ../desktop-environment/xmonad/polybar/default.nix
      ../desktop-environment/xmonad/screensaver.nix
      ../desktop-environment/xmonad/udiskie.nix
      ../desktop-environment/xmonad/xmonad/default.nix
    ];
  };
}
