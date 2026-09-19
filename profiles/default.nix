{
  cli = ./cli.nix;
  desktop-environment = import ./desktop-environment;
  development = import ./development;
  ebook-creation = ./ebook-creation.nix;
  emacs = ./emacs.nix;
  games = import ./games;
  general-desktop = ./general-desktop.nix;
  latex = ./latex.nix;
  multimedia = import ./multimedia;
  office = ./office.nix;
  writing = ./writing.nix;
}
