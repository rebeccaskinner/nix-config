{ config, pkgs, ... }:

let
  load             = f: import f { inherit pkgs utils; };
  utils            = import ./utils;
  games            = load ./collections/games;
  awsPkgs          = with pkgs; [ awscli2 aws-mfa ];
  workProductivity = [ pkgs.zoom pkgs.chromium ];
in
import ./generic.nix
  { desktopEnvironment = "kde";
    platform = "x86-64";

    extraEnvironments =
      [ (load ./configs/kitty.nix)];

    extraPackages = awsPkgs;

    developmentEnvironmentArgs = {
      haskell-formatter-package = ./development-environment/haskell/formatter/fourmolu.nix;
    };

    inherit config pkgs;
  }
