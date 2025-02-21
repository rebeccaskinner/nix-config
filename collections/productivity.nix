{utils, pkgs, ...}:
utils.env.packagesEnvironment (with pkgs; [
  bitwarden
  slack
  element-desktop
  thunderbird
  libreoffice
  signal-desktop
  kiwix
  kiwix-tools
  nextcloud-client
  super-productivity
  simplex-chat-desktop
])
