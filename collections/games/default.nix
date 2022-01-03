{utils, pkgs, ...}:
rec {
  terminalGames =  utils.callCollection ./terminal.nix { inherit pkgs; };
  graphicalGames = utils.callCollection ./graphical.nix { inherit pkgs; };
  dndTools = utils.callCollection ./DnD.nix { inherit pkgs; };
  allGames = utils.concatCollections [terminalGames graphicalGames dndTools];
}
