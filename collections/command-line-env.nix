{utils, pkgs, ...}:
let
  commandLineTools = with pkgs; [
    htop
    bat
    silver-searcher
    pulsemixer
    tmux
    file
    alsa-utils
    gifsicle
    ffmpeg
    gotop
    dnsutils
    bitwarden-cli
    neofetch
  ];
in utils.makeCollection commandLineTools