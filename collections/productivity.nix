{utils, pkgs, ...}:
utils.newCollection(with pkgs; [
    slack
    thunderbird
    todoist
    todoist-electron
    libreoffice
])
