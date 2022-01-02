let
  xmonadPackages = {pkgs, ...}:
    {
      home.packages =
        with pkgs; [
          scrot
          trayer
          pcmanfm
          networkmanagerapplet
          xmonad-log
        ];
    };
in
[ ./xmonad
  ./feh
  xmonadPackages
]
