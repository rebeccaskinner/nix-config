{utils, pkgs, ...}:
utils.env.packagesEnvironment (with pkgs; [
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
])
