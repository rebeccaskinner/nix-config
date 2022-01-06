{ pkgs
, utils
, ...}:
{
  packages =
    with pkgs; [
      gcc
      gnumake
      valgrind
      binutils
      elfutils
      gdb
      ctags
      ccls
    ];
  imports = [];
  emacsExtraPackages = (epkgs: with epkgs; [ccls]);
  emacsExtraConfig = ''
    (setq ccls-executable "${pkgs.ccls}/bin/ccls")
'';
}
