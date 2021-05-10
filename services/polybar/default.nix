# NB: Based off of https://gvolpe.com/blog/xmonad-polybar-nixos/
# https://github.com/gvolpe/nix-config/tree/master/home/services/polybar
# for more examples

{ config, pkgs, ... }:
  let
    polybarService = pkgs.polybar.override {
      githubSupport = true;
      pulseSupport = true;
    };

    xmonad = ''
[module/xmonad]
type = custom/script
exec = ${pkgs.xmonad-log}/bin/xmonad-log
tail = true
'';

  in
    {
      services.polybar = {
        enable = true;
        package = polybarService;
        config = ./config.ini;
        extraConfig = xmonad;
        script = ''
polybar top & disown
'';
      };
    }
