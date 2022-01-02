{ config, pkgs, ...}:
{
  xsession = {
    enable = true;
    pointerCursor = {
      package = pkgs.vanilla-dmz;
      defaultCursor = "left_ptr";
      name = "Vanilla-DMZ";
    };
    initExtra = ''
xrandr --dpi 90
xrandr --output eDP-1-1 --brightness 0.2
xrandr --output eDP-1-1 --off
xrandr --output DP-5 --left-of HDMI-0
# xrandr --output HDMI-0 --auto
# xrandr --output HDMI-0 --right-of DP-5
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
