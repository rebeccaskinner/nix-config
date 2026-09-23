{ pkgs
, utils
, ...}:
{
  packages = with pkgs; [nix-prefetch-scripts];
  imports = [];
  emacsExtraPackages = (epkgs: with epkgs; [nix-buffer nix-sandbox nix-mode]);
  emacsExtraConfig = builtins.readFile ./nixdev.el;
}
