# XMonad desktop: X server, sddm, and the tray applets, compositor, bar,
# launcher, and notification daemon that make a bare window manager livable.
{ pkgs, primaryUser, inputs, system, ... }:
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
      pcmanfm
      thunar
      tumbler
      xcursor-themes
    ];
  home-manager.users.${primaryUser} = {
    imports = [
      ../../desktop-environment/xmonad/blueman.nix
      ../../desktop-environment/xmonad/dunst.nix
      ../../desktop-environment/xmonad/feh.nix
      ../../desktop-environment/xmonad/mimeApps.nix
      ../../desktop-environment/xmonad/network-manager-applet.nix
      ../../desktop-environment/xmonad/picom.nix
      ../../desktop-environment/xmonad/polybar/default.nix
      ../../desktop-environment/xmonad/screensaver.nix
      ../../desktop-environment/xmonad/udiskie.nix
      ../../desktop-environment/xmonad/xmonad/default.nix
    ];

    programs.rofi = {
      enable = true;
      terminal = "${pkgs.kitty}/bin/kitty";
      theme = ../../configs/rofi/themes/darkplum.rasi;
      plugins = with pkgs; [
        rofi-emoji
        rofi-calc
        inputs.rofi-hoogle.packages.${system}.rofi-hoogle
      ];
    };
  };
}
