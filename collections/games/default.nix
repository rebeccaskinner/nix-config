{ pkgs, utils, ...}:
rec {
  terminalGames =  import ./terminal.nix { inherit pkgs utils; };
  graphicalGames = import ./graphical.nix { inherit pkgs utils; };
#  dndTools = import ./DnD.nix { inherit pkgs utils; };
  allGames = utils.env.concatEnvironments [terminalGames graphicalGames];
}
