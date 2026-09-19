# Media playback: local files, discs, streaming from the media server, and
# fetching video from the web.
#
# Commercial blu-rays need a libbluray with AACS and BD+ support, which is
# nixpkgs' libbluray-full. vlc already uses it; mpv is rebuilt against it
# here. Both still need a KEYDB.cfg in ~/.config/aacs to actually decrypt.
{ pkgs, primaryUser, ... }:
let
  mpv = pkgs.mpv.override {
    mpv-unwrapped = pkgs.mpv-unwrapped.override {
      libbluray = pkgs.libbluray-full;
    };
  };
in
{
  users.users.${primaryUser}.packages = [
    mpv
  ] ++ (with pkgs; [
    jellyfin-media-player
    vlc
    yt-dlp
  ]);
}
