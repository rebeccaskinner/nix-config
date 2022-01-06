{ pkgs
, utils
, ...
}:
let
  cfg = p: utils.env.configOnlyEnvironment (import p);
  mkConfigs = cfgPaths: utils.env.concatEnvironments (builtins.map cfg cfgPaths);

  mkImport = p: utils.env.importOnlyEnvironment (import p);
  mkImports = importPaths: utils.env.concatEnvironments (builtins.map mkImport importPaths);

  generalConfigs =
    mkConfigs [ ./bash.nix
                ./dircolors.nix
                ./direnv.nix
                ./git.nix
                ./gpg-agent.nix
                ./gpg.nix
                ./kdeconnect.nix
              ];

  generalImports = mkImport ./gtk.nix;
in utils.env.concatEnvironments [ generalConfigs generalImports ]
