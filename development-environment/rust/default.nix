{ pkgs
, utils
, extraLibs ? []
, ...
}:
{
  packages =
    with pkgs; [
      rustc
      rustfmt
      rust-analyzer
      cargo
      cargo-edit
    ];
  imports = [];
  emacsExtraPackages =
    (epkgs: with epkgs; [rustic cargo]);
  emacsExtraConfig = builtins.readFile ./rust.el;
}
