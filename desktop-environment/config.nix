{ pkgs, utils, desktopEnvironment, ...}:
let
  configs =
    { gnome = import ./gnome { inherit pkgs utils; };
      xmonad = import ./xmonad { inherit pkgs utils; };
      generic-desktop-environment = import ./generic-desktop-environment { inherit pkgs utils; };
    };
  desktopEnvConfig = configs."${desktopEnvironment}";
  xserverTools = import ./xserverTools.nix { inherit pkgs utils; };
in utils.env.concatEnvironments [xserverTools desktopEnvConfig]
