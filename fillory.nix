{ config, pkgs, cudaPkgs, inputs, system, ... }:

let
  load     = f: import f { inherit pkgs utils; };
  utils    = import ./utils;
  games    = load ./collections/games;
in
import ./generic.nix
  { desktopEnvironment = "gnome";
    platform = "x86-64";
    extraEnvironments = [ (load ./configs/kitty.nix)
                          games.allGames
                        ];
    developmentEnvironmentArgs = {
      haskell-formatter-package = ./development-environment/haskell/formatter/fourmolu.nix;
    };
    inherit config pkgs cudaPkgs inputs system;
  }
