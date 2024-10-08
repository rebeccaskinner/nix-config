{utils, pkgs, ...}:
let
  libbluray = pkgs.libbluray.override {
    withAACS = true;
    withBDplus = true;
    withJava = true;
  };
  vlc = pkgs.vlc.override { inherit libbluray; };
  whisper-cpp = pkgs.callPackage ./whisper-cpp { nvidia_x11 = pkgs.linuxPackages.nvidia_x11; gcc = pkgs.gcc11; };
in
utils.env.packagesEnvironment ([vlc libbluray] ++ (with pkgs;
  [ makemkv
    mkvtoolnix
    handbrake
    ffmpeg
    kazam
    rhythmbox
    jellyfin-media-player
    yt-dlp
    mat2
    whisper-cpp
    k3b
    ccextractor
    jdk17
  ]))
