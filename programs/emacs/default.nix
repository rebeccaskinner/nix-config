{pkgs, ...}:
{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.ace-window
      epkgs.flycheck
      epkgs.direnv
      epkgs.fill-column-indicator
      epkgs.dante
      epkgs.auto-complete
      epkgs.pdf-tools
      epkgs.dhall-mode
      epkgs.proof-general
      epkgs.rebecca-theme
      epkgs.nix-mode
      epkgs.magit
      epkgs.ox-gfm
      epkgs.nix-buffer
      epkgs.nix-sandbox
      epkgs.restclient
      epkgs.protobuf-mode
      epkgs.darcula-theme
      epkgs.format-sql
      epkgs.paredit
      epkgs.ox-pandoc
      epkgs.json-mode
      epkgs.graphviz-dot-mode
      epkgs.go-mode
      epkgs.go-playground
      epkgs.expand-region
      epkgs.cargo
      epkgs.rainbow-delimiters
      epkgs.sql-indent
      epkgs.spacemacs-theme
      epkgs.use-package
      epkgs.pdf-tools
    ];
  };
  home.file.".emacs.d" = {
    source = ./emacs.d;
    recursive = true;
  };
}
