# Basic C/C++ toolchain for hacking on small projects outside of a
# project-specific nix environment.
{ pkgs, primaryUser, ... }:
{
  users.users.${primaryUser}.packages = with pkgs; [
    ccls
    gcc
    gdb
    gnumake
    universal-ctags
    valgrind
  ];
}
