# Writing documents in LaTeX, and converting to and from it.
{ pkgs, primaryUser, ... }:
{
  users.users.${primaryUser}.packages = with pkgs; [
    pandoc
    python3Packages.pygments
    texliveFull
  ];
}
