{
  xsession = {
    enable = true;
    initExtra = ''
xsetroot -cursor_name left_ptr
xrandr --dpi 90
xrandr --output eDP-1-1 --brightness 0.2
xrandr --output eDP-1-1 --off
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
      };
    };
  };
}
