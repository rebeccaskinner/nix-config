{ config
, pkgs
, desktopEnvironment # should be one of "kde" or "xmonad"
, platform # should be one of "x86-64" or "arm64"
, haskellVersion ? null
, extraImports  ? []
, extraPackages ? []
, extraEnvironments ? []
, overlays ? []
, developmentEnvironmentArgs ? {}
}:

let
  utils = import ./utils;
  environment =
    let
      usrEnvs = utils.env.concatEnvironments extraEnvironments;
      envPackages = import ./collections/system-defaults.nix {inherit pkgs utils platform; };
      devEnvironment = import ./development-environment ({ inherit pkgs utils haskellVersion; } // developmentEnvironmentArgs);
      de = import ./desktop-environment/config.nix { inherit pkgs utils desktopEnvironment; };
      defaultEnvironment = import ./configs/generic.nix { inherit pkgs utils; };
      e = utils.env.concatEnvironments [ devEnvironment de defaultEnvironment envPackages ];
      emacsEnvironment = import ./emacs { inherit pkgs utils;
                                          extraPackages = e.emacsExtraPackages;
                                          extraConfigs = [e.emacsExtraConfig];
                                        };
    in utils.env.concatEnvironments [e emacsEnvironment usrEnvs];

  homeConfig =
    {
      # Let Home Manager install and manage itself.
      programs.home-manager.enable = true;

      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays = overlays;

      # Home Manager needs a bit of information about you and the
      # paths it should manage.
      home.username = "rebecca";
      home.homeDirectory = "/home/rebecca";
      imports = environment.imports ++ extraImports;
      home.packages = environment.packages ++ extraPackages;
      home.keyboard.options = ["ctrl:nocaps"];

      # This value determines the Home Manager release that your
      # configuration is compatible with. This helps avoid breakage
      # when a new Home Manager release introduces backwards
      # incompatible changes.
      #
      # You can update Home Manager without changing this value. See
      # the Home Manager release notes for a list of state version
      # changes in each release.
      home.stateVersion = "21.03";
    };
in homeConfig
