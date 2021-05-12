{pkgs, ...}:
{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      ace-window
      flycheck
      direnv
      fill-column-indicator
      dante
      auto-complete
      pdf-tools
      dhall-mode
      proof-general
      rebecca-theme
      nix-mode
      magit
      ox-gfm
      nix-buffer
      nix-sandbox
      restclient
      protobuf-mode
      darcula-theme
      format-sql
      paredit
      ox-pandoc
      json-mode
      graphviz-dot-mode
      go-mode
      go-playground
      expand-region
      cargo
      rainbow-delimiters
      sql-indent
      spacemacs-theme
      use-package
      pdf-tools
      goto-last-change
    ];
  };
  home.file.".emacs.d" = {
    source = ./emacs.d;
    recursive = true;
  };
}
