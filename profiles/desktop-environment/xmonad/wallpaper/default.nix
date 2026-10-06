{pkgs, ...}:
let
  wallpaper = ./wallpaper.png;
in
{
  xsession = {
    enable = true;

    initExtra = ''
      ${pkgs.feh}/bin/feh --bg-scale ${wallpaper}
    '';
  };
}
