{utils, config, pkgs, ...}:

[
  (utils.addPackageCollection ./haskell {})
  (utils.addPackageCollection ./rust {})
  (utils.addPackageCollection ./gcc {})
  (utils.addPackageCollection ./devtools {})
]
