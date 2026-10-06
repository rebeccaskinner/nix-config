{ pkgs
, rofi-hoogle
, system
, ... }:
{
programs.rofi = {
      enable = true;
      settings.terminal = "${pkgs.kitty}/bin/kitty";
      theme = ./themes/darkplum.rasi;
      plugins = with pkgs; [
        rofi-emoji
        rofi-calc
        rofi-hoogle.packages.${system}.rofi-hoogle
      ];
    };
}
