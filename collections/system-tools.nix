{utils, pkgs, ...}:
utils.env.packagesEnvironment (with pkgs; [
  baobab
  wireshark
  shellcheck
])
