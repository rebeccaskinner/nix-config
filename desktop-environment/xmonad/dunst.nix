{ config, pkgs, ... }:

{
  services.dunst = {
    enable = true;
    iconTheme = {
      name = "candy-icons";
      package = pkgs.candy-icons;
      size = "16x16";
    };
    settings = {
      global = {
        monitor = 0;
        geometry = "600x50-50+65";
        shrink = "yes";
        transparency = 10;
        padding = 16;
        horizontal_padding = 16;
        font = "FiraCode 10";
        line_height = 4;
        format = ''<b>%s</b>\n%b'';
      };
    };
  };
}
