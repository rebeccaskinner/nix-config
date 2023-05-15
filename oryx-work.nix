{ config, pkgs, ... }:

let
  load  = f: import f { inherit pkgs utils; };
  utils = import ./utils;
in
import ./generic.nix
  { desktopEnvironment = "xmonad";
    platform = "x86-64";
    extraEnvironments =
      [ (load ./configs/kitty.nix)];

    extraPackages =
      (with pkgs; [ awscli2 aws-mfa zoom-us chromium remmina freerdpUnstable libva libva-utils jellyfin-media-player]);

    developmentEnvironmentArgs = {
      haskell-formatter-package = ./development-environment/haskell/formatter/fourmolu.nix;
    };

    inherit config pkgs;
  }
