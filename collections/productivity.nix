{utils, pkgs, ...}:
utils.env.packagesEnvironment (with pkgs; [
  bitwarden
  slack
  thunderbird
  todoist
  todoist-electron
  libreoffice
  signal-desktop
])
