{ platform, utils, pkgs, ...}:
let
  globalDefaults = utils.env.concatEnvironments [
    (import ./command-line-env.nix { inherit pkgs utils; })
    (import ./fonts.nix { inherit pkgs utils; })
    (import ./system-tools.nix { inherit pkgs utils; })
  ];

  x86-64-pkgs = utils.env.concatEnvironments [
    (import ./graphics.nix { inherit utils pkgs; })
    (import ./productivity.nix { inherit utils pkgs; })
    (import ./multimedia.nix { inherit utils pkgs; })
    (import ./writing-tools.nix { inherit utils pkgs; })
  ];

  platformMap =
    { x86-64 = utils.env.mergeEnvironments x86-64-pkgs globalDefaults;
      arm64 = globalDefaults;
    };
in platformMap.${platform}
