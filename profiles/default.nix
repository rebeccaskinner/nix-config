{
  ai-assistants = ./ai-assistants.nix;
  cli = ./cli/default.nix;
  desktop-environment = import ./desktop-environment;
  development = import ./development;
  ebook-creation = ./ebook-creation.nix;
  emacs = ./emacs/default.nix;
  games = import ./games;
  general-desktop = ./general-desktop/default.nix;
  gnu = ./gnu.nix;
  latex = ./latex.nix;
  multimedia = import ./multimedia;
  office = ./office.nix;
  writing = ./writing.nix;
}
