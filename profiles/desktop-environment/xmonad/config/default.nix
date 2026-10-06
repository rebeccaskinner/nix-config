{ config, pkgs, ...}:
{
  windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = xmonadPackage: with xmonadPackage; [
      aeson
      dbus
      monad-logger
      xmonad-contrib
      xmonad-extras
    ];
    config = ./xmonad.hs;
    libFiles = {
      "Polybar.hs" = ./Polybar.hs;
      "ColorType.hs" = ./ColorType.hs;
      "ColorX11.hs" = ./ColorX11.hs;
      "XmonadTheme.hs" = ./XmonadTheme.hs;
    };
  };
}
