{pkgs, ...}:
{
  programs.emacs = {
    enable = true;
    overrides = self: super: rec {
      darkplum-theme = self.melpaBuild rec {
        name = "darkplum-theme";
        pname = "darkplum-theme";
        version = "1.0.0";
        src = pkgs.fetchFromGitHub {
          owner = "rebeccaskinner";
          repo = "darkplum-theme";
          rev = "03ceef524e99750b530096121c4d2ffacbab97eb";
          sha256 = "1495dbz2p4p1hwgfbkjhmzxbwlvxi1wzr71p84jkmc8177jp4q52";
        };
        buildInputs = [];
        recipe = pkgs.writeText "recipe" ''
          (darkplum-theme
          :repo "rebeccaskinner/darkplum-theme"
          :fetcher github
          :files ("darkplum-theme.el"))
        '';
        meta = {
          description = "A dark purple theme for emacs";
          longDescription = "A dark purple theme for emacs";
        };
        license = pkgs.lib.licenses.gpl3Plus.spdxId;
      };
    };
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
      darkplum-theme
      rebecca-theme
    ];
  };
  home.file.".emacs.d" = {
    source = ./emacs.d;
    recursive = true;
  };
}
