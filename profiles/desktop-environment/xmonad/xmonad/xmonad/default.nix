{ config, pkgs, ...}:
{
  home.pointerCursor = {
    enable = true;
    x11.enable = true;
    x11.defaultCursor = "left_ptr";
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
  };
  xsession = {
    enable = true;

    initExtra = ''
feh --bg-scale /home/rebecca/.config/wallpaper
'';
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = xmonadPackage: with xmonadPackage; [
        aeson
        dbus
        monad-logger
        xmonad-contrib
      ];
      config = ./xmonad.hs;
      libFiles = {
        "Polybar.hs" = ./Polybar.hs;
        "ColorType.hs" = ./ColorType.hs;
        "ColorX11.hs" = ./ColorX11.hs;
        "XmonadTheme.hs" = ./XmonadTheme.hs;
      };
    };
  };
}
