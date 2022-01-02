{ utils, desktopEnvironment, ...}:
let
  configs =
    { kde = import ./kde;
      xmonad = import ./xmonad;
    };
  desktopEnvConfig = configs."${desktopEnvironment}";
  xserverTools = import ./xserverTools.nix;
in utils.cons xserverTools desktopEnvConfig
