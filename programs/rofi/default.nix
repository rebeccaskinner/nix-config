{ pkgs, ... }:
let
  rofi-hoogle-src = pkgs.fetchFromGitHub {
    owner = "rebeccaskinner";
    repo = "rofi-hoogle";
    rev = "27c273ff67add68578052a13f560a08c12fa5767";
    sha256 = "09vx9bc8s53c575haalcqkdwy44ys1j8v9k2aaly7lndr19spp8f";
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
