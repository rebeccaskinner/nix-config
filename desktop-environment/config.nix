{ desktopEnvironment, ...}:
let
  configs =
    { kde = import ./kde;
      xmonad = import ./xmonad;
    };
in configs."${desktopEnvironment}"
