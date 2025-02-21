{utils, pkgs, cudaPkgs, ...}:
let
  libbluray = pkgs.libbluray.override {
    withAACS = true;
    withBDplus = true;
    withJava = true;
  };
  vlc = pkgs.vlc.override { inherit libbluray; };
  whisper-cpp = cudaPkgs.callPackage ./whisper-cpp { nvidia_x11 = cudaPkgs.linuxPackages.nvidia_x11; gcc = cudaPkgs.gcc11; };
in
utils.env.simpleEnvironment {
  packages = [vlc libbluray] ++ (with pkgs;
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
      ccextractor

      # audio cd ripping
      cdparanoiaIII
      whipper
      abcde

      # ebook digitizing
      scantailor-advanced
      tesseract
      pdftk
      ghostscript
      calibre
    ]);
  imports = [(import ./java.nix { inherit pkgs; })];
}
