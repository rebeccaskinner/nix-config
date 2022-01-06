{ config, pkgs, ... }:

let
  load     = f: import f { inherit pkgs utils; };
  defaults = import ./collections/defaults/system-defaults.nix;
  configs  = import ./configs/defaults/system-defaults.nix;
  utils    = import ./utils;
  games    = load ./collections/games;

in
import ./generic.nix
  { desktopEnvironment = "kde";
    platform = "x86-64";
    extraEnvironments = [ (load ./configs/kitty.nix)
                          games.allGames
                        ];
    inherit config pkgs;
  }
