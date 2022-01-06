{ pkgs, utils, desktopEnvironment, ...}:
let
  configs =
    { kde = import ./kde { inherit pkgs utils; };
      xmonad = import ./xmonad { inherit pkgs utils; };
    };
  desktopEnvConfig = configs."${desktopEnvironment}";
  xserverTools = import ./xserverTools.nix { inherit pkgs utils; };
in utils.env.concatEnvironments [xserverTools desktopEnvConfig]
