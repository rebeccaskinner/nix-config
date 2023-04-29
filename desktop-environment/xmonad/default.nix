{ pkgs
, utils
, ...
}:
let
  cfg = p: utils.env.configOnlyEnvironment (import p);
  mkConfigs = cfgPaths: utils.env.concatEnvironments (builtins.map cfg cfgPaths);

  mkImport = p: utils.env.importOnlyEnvironment (import p);
  mkImports = importPaths: utils.env.concatEnvironments (builtins.map mkImport importPaths);

  xmonadGeneralEnv =
    mkConfigs [ ./feh.nix
                ./blueman.nix
                ./mimeApps.nix
                ./network-manager-applet.nix
                ./picom.nix
                ./udiskie.nix
                ./screensaver.nix
              ];

  xmonadImports = mkImports [ ./xmonad ./dunst.nix ./polybar ];

  xmonadPackages = utils.env.packagesEnvironment (with pkgs; [ pcmanfm ]);

in utils.env.concatEnvironments [xmonadImports xmonadGeneralEnv xmonadPackages]
