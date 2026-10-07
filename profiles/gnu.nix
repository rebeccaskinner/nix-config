# GNU userland. Mostly useful on macOS, where it replaces the BSD versions of
# the standard tools so that scripts and aliases (e.g. `ls --color`) behave
# the same as on linux. Harmless, if redundant, on NixOS.
{ pkgs, primaryUser, ... }:
{
  users.users.${primaryUser}.packages = with pkgs; [
    coreutils
    diffutils
    findutils
    gawk
    getopt
    gnugrep
    gnupatch
    gnused
    gnutar
    gzip
    less
    which
  ];
}
