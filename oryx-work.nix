{ config, pkgs, ... }:

let
  load  = f: import f { inherit pkgs utils; };
  utils = import ./utils;
in
import ./generic.nix
  { desktopEnvironment = "xmonad";
    platform = "x86-64";
    haskellVersion = 924;
    extraEnvironments =
      [ (load ./configs/kitty.nix)];

    extraPackages =
      (with pkgs; [ konsole awscli2 aws-mfa zoom-us chromium remmina freerdpUnstable libva libva-utils jellyfin-media-player]);

    developmentEnvironmentArgs = {
      haskell-formatter-package = ./development-environment/haskell/formatter/fourmolu.nix;
    };

    rofi-term = "${pkgs.konsole}/bin/konsole";

    inherit config pkgs;
  }
