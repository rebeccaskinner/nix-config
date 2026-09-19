# Transcoding and remuxing video, and generating subtitles.
#
# whisper-cpp comes from `cudaPkgs` when a host passes one through
# specialArgs, so subtitle transcription can use the GPU; otherwise it falls
# back to the CPU build from `pkgs`.
{ pkgs, primaryUser, ... }@args:
let
  cudaPkgs = args.cudaPkgs or pkgs;
in
{
  users.users.${primaryUser}.packages = (with pkgs; [
    ab-av1
    ffmpeg
    ffsubsync
    handbrake
    mediainfo
    mkvtoolnix
    sox
  ]) ++ [
    cudaPkgs.whisper-cpp
  ];
}
