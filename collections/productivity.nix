{utils, pkgs, ...}:
utils.env.packagesEnvironment (with pkgs; [
  bitwarden
  slack
  thunderbird
  libreoffice
  signal-desktop
  kiwix
  kiwix-tools
  nextcloud-client
  super-productivity
])
