# Utilities for writing and debugging nix packages and expressions.
{ pkgs, primaryUser, ... }:
{
  users.users.${primaryUser}.packages = with pkgs; [
    deadnix
    nil
    nix-diff
    nix-index
    nix-prefetch-scripts
    nix-tree
    nixfmt
    nurl
    statix
  ];
}
