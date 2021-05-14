# NB: Based off of https://gvolpe.com/blog/xmonad-polybar-nixos/
# https://github.com/gvolpe/nix-config/tree/master/home/services/polybar
# for more examples

{ config, pkgs, ... }:
  let
    polybarService = pkgs.polybar.override {
      githubSupport = true;
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
    openGithub = "${xdgUtils}/bin/xdg-open https\\://github.com/notifications";

    xmonad = ''
      [module/xmonad]
      type = custom/script
      exec = ${pkgs.xmonad-log}/bin/xmonad-log
      tail = true
    '';

    github = ''
      [module/github]
      type = internal/github
      token = ''${file:${config.xdg.configHome}/polybar/github-notifications-token}
      user = rebeccaskinner
      label = %{A1:${openGithub}:}  %notifications%%{A}
      empty-notifications = true
    '';
  in
    {
      xdg.configFile."polybar/github-notifications-token".source = "${config.xdg.configHome}/credentials/github-notification-token";
      services.polybar = {
        enable = true;
        package = polybarService;
        config = ./config.ini;
        extraConfig = xmonad + github;
        script = ''polybar top & disown'';
      };
    }
