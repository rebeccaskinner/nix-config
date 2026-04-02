{pkgs, utils, ...}:
let
  defaultGlobalEnv =
    { packages = with pkgs; [ curl httpie jq s3cmd];
      imports = [];
      emacsExtraPackages = utils.function.const [];
      emacsExtraConfig = "";
    };

in utils.env.concatEnvironments [ defaultGlobalEnv ]
