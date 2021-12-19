{ pkgs, ... }:
let
  rofi-hoogle-src = pkgs.fetchFromGitHub {
    owner = "rebeccaskinner";
    repo = "rofi-hoogle";
    rev = "1694dba8de5af19e703357492d036568e715a449";
    sha256 = "12fzyaaf176mvpj7cmaxvzq2a3krd4wr26r7vl59zsyrlrj2y810";
  };
  rofi-hoogle = import "${rofi-hoogle-src}/release.nix";
in
{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.kitty}/bin/kitty";
    theme = ./themes/darkplum.rasi;
    plugins = with pkgs; [
      rofi-emoji
      rofi-calc
      rofi-hoogle.rofi-hoogle
    ];
  };
}
