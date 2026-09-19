# Ripping optical media: blu-ray and DVD backups, subtitle extraction from
# closed captions, and CD ripping and tagging.
{ pkgs, primaryUser, ... }:
let
  video = with pkgs; [
    ccextractor
    dvdbackup
    ffmpeg
    kdePackages.k3b
    libdvdcss
    lsdvd
    makemkv
    mediainfo
    mkvtoolnix
  ];

  audio = with pkgs; [
    abcde
    cdparanoiaIII
    flac
    picard
    whipper
  ];
in
{
  users.users.${primaryUser}.packages = video ++ audio;
}
