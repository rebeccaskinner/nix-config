{utils, pkgs, ...}:
let
  libbluray = pkgs.libbluray.override {
    withAACS = true;
    withBDplus = true;
  };
  vlc = pkgs.vlc.override { inherit libbluray; };
in
utils.env.packagesEnvironment ([vlc libbluray] ++ (with pkgs;
  [ makemkv
    handbrake
    mplayer
    ffmpeg
    kazam
    cozy
    rhythmbox
    jellyfin-media-player
    yt-dlp
  ]))
