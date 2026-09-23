# Transcoding and remuxing video, and syncing subtitles.
#
# Subtitle generation is deliberately not here: whisper-cpp is a heavy
# dependency for something used rarely. Reach for it on demand with
# `nix run nixpkgs#whisper-cpp` instead.
{ pkgs, primaryUser, ... }:
{
  users.users.${primaryUser}.packages = with pkgs; [
    ab-av1
    ffmpeg
    ffsubsync
    handbrake
    mediainfo
    mkvtoolnix
    sox
  ];
}
