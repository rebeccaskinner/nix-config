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
    mkConfigs [ ../xmonad/feh.nix
                ../xmonad/blueman.nix
                ../xmonad/mimeApps.nix
                ../xmonad/network-manager-applet.nix
                ../xmonad/picom.nix
                ../xmonad/udiskie.nix
                ../xmonad/screensaver.nix
              ];

  xmonadPackages = utils.env.packagesEnvironment (with pkgs; [ pcmanfm ]);

in utils.env.concatEnvironments [xmonadGeneralEnv xmonadPackages]
