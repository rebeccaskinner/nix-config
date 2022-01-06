{ pkgs
, utils
, ...}:
{
  packages = with pkgs; [nix-prefetch-scripts rnix-lsp];
  imports = [];
  emacsExtraPackages = (epkgs: with epkgs; [nix-buffer nix-sandbox nix-mode]);
  emacsExtraConfig = builtins.readFile ./nixdev.el;
}
