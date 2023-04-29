# NB: Based off of https://gvolpe.com/blog/xmonad-polybar-nixos/
# https://github.com/gvolpe/nix-config/tree/master/home/services/polybar
# for more examples

{ config, pkgs, ... }:
  let
    polybarService = pkgs.polybar.override {
      pulseSupport = true;
    };
    xdgUtils = pkgs.xdg_utils.overrideAttrs (
      old: {
        nativeBuildInputs = old.nativeBuildInputs or [] ++ [ pkgs.makeWrapper ];
        postInstall = old.postInstall + "\n" + ''
        wrapProgram $out/bin/xdg-open --suffix PATH : /run/current-system/sw/bin --suffix BROWSER : ${browser}
        '';
      }
    );
    browser = "${pkgs.firefox-beta-bin}/bin/firefox";
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
        script = ''polybar top & disown'';
      };
    }
