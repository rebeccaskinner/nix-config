{ pkgs, utils, rofi-hoogle-plugin, ... }:
utils.env.importOnlyEnvironment ({
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.kitty}/bin/kitty";
    theme = ./themes/darkplum.rasi;
    plugins = with pkgs; [
      rofi-emoji
      rofi-calc
      rofi-hoogle-plugin
    ];
  };
})
