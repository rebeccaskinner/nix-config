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
                ./udiskie.nix ];

  xmonadImports = mkImports [ ./xmonad ./dunst.nix ./polybar ];

in utils.env.concatEnvironments [xmonadImports xmonadGeneralEnv]
