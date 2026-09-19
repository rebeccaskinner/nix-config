# Open source games, plus prismlauncher, which is an open source launcher
# for a game that is not.
{ pkgs, primaryUser, ... }:
let
  terminal = with pkgs; [
    bastet
    nethack
    ninvaders
    nsnake
  ];

  kde = with pkgs.kdePackages; [
    bomber
    kbounce
    kbreakout
    kolf
    kollision
    kreversi
    ksnakeduel
  ];

  desktop = with pkgs; [
    cataclysm-dda
    lbreakouthd
    luanti
    neverball
    openttd
    prismlauncher
    supertuxkart
    wesnoth
    xmoto
  ];
in
{
  users.users.${primaryUser}.packages = terminal ++ kde ++ desktop;
}
