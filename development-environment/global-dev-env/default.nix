{pkgs, utils, ...}:
let
  defaultGlobalEnv =
    { packages = with pkgs; [ curl httpie jq s3cmd];
      imports = [];
      emacsExtraPackages = utils.function.const [];
      emacsExtraConfig = "";
    };

  gitEnv =
    { packages = with pkgs; [ gitg ];
      imports = [ (utils.constImport ./git.nix) ];
      emacsExtraPackages = epkgs: [epkgs.magit];
      emacsExtraConfig = "";
    };


in utils.env.concatEnvironments [ defaultGlobalEnv gitEnv ]
