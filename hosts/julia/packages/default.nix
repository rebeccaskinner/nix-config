{ config, pkgs, pkgsStable, ... }:
let
  basicPackages = with pkgs; [
    bat
    pulsemixer
    file
    alsa-utils
    gifsicle
    dnsutils
    bitwarden-cli
    ripgrep
    unzip
    vim
    renameutils
    rename
    graphicsmagick
    mat2
  ];

  multimedia =
    let
      libbluray = pkgs.libbluray.override {
        withAACS = true;
        withBDplus = true;
        withJava = true;
      };

      vlc = pkgs.vlc.override { inherit libbluray; };
    in [
      vlc
      libbluray
      pkgs.mkvtoolnix
      pkgs.ffmpeg
      pkgs.jellyfin-media-player
      pkgs.yt-dlp
    ];

  applications = with pkgs; [
    baobab # disk usage visualization
    wireshark # network traffic
    gimp # image editing
    drawio # diagrams
    qiv # image viewer
    bitwarden-desktop # password manager
    slack # communications
    thunderbird # email
    libreoffice # office suite
    signal-desktop # messaging
    kiwix # offline website archive
    kiwix-tools # tools for kiwix
    kazam # screen recording
    (pkgs.aspellWithDicts(d: with d;[en en-computers en-science]))
    pandoc # document conversion
    ispell # spell checking
    texliveFull
    python3Packages.pygments # syntax highlighting
    evince # document viewer
  ];

  devPackages = with pkgs; [
    shellcheck
    nix-index
    curl
    httpie
    jq
    s3cmd
  ];

in
{
  users.users.rebecca.defaults.packages = builtins.concatLists [
    basicPackages
    multimedia
    applications
    devPackages
  ];
}
