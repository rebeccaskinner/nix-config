{ pkgs
, pkgs-stable
, utils
, inputs
, system
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
                ./ssh-agent.nix
                ./gpg.nix
                # ./kdeconnect.nix
                ./fzf.nix
              ];

  generalImports = mkImport ./gtk.nix;

  rofi = import ./rofi
    { rofi-hoogle-plugin = inputs.rofi-hoogle.outputs.packages.${system}.rofi-hoogle;
      inherit pkgs utils;
    };

  tmux = import ./tmux.nix { inherit pkgs utils; };
in utils.env.concatEnvironments [generalConfigs generalImports rofi tmux]
