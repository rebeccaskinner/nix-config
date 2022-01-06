{utils, pkgs, ...}:
utils.env.packagesEnvironment (with pkgs; [
    slack
    thunderbird
    todoist
    todoist-electron
    libreoffice
])
