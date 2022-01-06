{haskellPackages}:
rec {
  package = haskellPackages.fourmolu;

  config = {...}: {
    xdg = {
      configFile = {
        "fourmolu.yaml" = {
          source = ./fourmolu.yaml;
          recursive = false;
        };
      };
    };
  };

  exec = "${package}/bin/fourmolu";
}
