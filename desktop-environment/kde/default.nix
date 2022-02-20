{ pkgs
, utils
, ...
}:
utils.env.packagesEnvironment [pkgs.latte-dock]
