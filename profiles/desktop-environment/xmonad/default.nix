# XMonad desktop: X server, sddm, and the tray applets, compositor, bar,
# launcher, and notification daemon that make a bare window manager livable.
{ pkgs, primaryUser, system, ... }:
let
  xmonadSource = ./config;
in
{
  services = {
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };
    blueman = {
      enable = true;
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
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        enableConfiguredRecompile = true;
        extraPackages = xmonadPkgs: with xmonadPkgs; [
          aeson
          dbus
          monad-logger
        ];
        config = ./config/xmonad.hs;
        ghcArgs = [
          "-i${xmonadSource}/lib"
        ];
      };
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
    xsession = {
      enable = true;
      initExtra = ''
        # Replace any stale xmonad config with the canonical source from
        # the nix store.
        rm -rf -- "$HOME/.xmonad"
        mkdir -p "$HOME/.xmonad"
        cp -R ${xmonadSource}/. "$HOME/.xmonad/"
        chmod -R u+w "$HOME/.xmonad"
      '';
    };
    imports = [
      ./blueman.nix
      ./dunst.nix
      ./feh.nix
      ./mimeApps.nix
      ./network-manager-applet.nix
      ./picom.nix
      ./polybar/default.nix
      ./rofi/default.nix
      ./screensaver.nix
      ./udiskie.nix
      ./cursor.nix
      ./wallpaper/default.nix
      ./xmonad/default.nix
    ];
  };
}
