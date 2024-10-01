{utils, pkgs, ...}:
utils.env.packagesEnvironment (with pkgs; [
    htop
    bat
    pulsemixer
    file
    alsa-utils
    gifsicle
    dnsutils
    bitwarden-cli
    neofetch
    ripgrep
    unzip
    renameutils
    rename
])
