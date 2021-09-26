{pkgs, ...}:
{
  programs.emacs = {
    enable = true;
    overrides = self: super: rec {
      darkplum-theme = self.melpaBuild rec {
        name = "darkplum-theme";
        pname = "darkplum-theme";
        version = "0.0.2";
        src = pkgs.fetchFromGitHub {
          owner = "rebeccaskinner";
          repo = "darkplum-theme";
          rev = "7a93038ede49c1c30e540d510d54ef83e00c2bce";
          sha256 = "0rsfvx8nsq9sbkdzs4mjqfwsqdwg5zz9y4wyqq05pamdhchmy2al";
        };
        recipe = pkgs.writeText "recipe" ''
          (darkplum-theme
          :repo "rebeccaskinner/darkplum-theme"
          :fetcher github)
        '';
        installPhase = ''
          runHook preInstall
          archive="$NIX_BUILD_TOP/source/$ename.el"
          if [ ! -f "$archive" ]; then
              echo "archive not found ($archive)"
              archive="$NIX_BUILD_TOP/packages/$ename-$version.tar"
          fi
          emacs --batch -Q \
              -l "$elpa2nix" \
              -f elpa2nix-install-package \
              "$archive" "$out/share/emacs/site-lisp/elpa"
          runHook postInstall
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
      nix-mode
      magit
      ox-gfm
      nix-buffer
      nix-sandbox
      restclient
      protobuf-mode
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
      use-package
      pdf-tools
      goto-last-change
      darkplum-theme
      hasklig-mode
      yaml-mode
      counsel
      counsel-org-clock
      counsel-tramp
      counsel-etags
      counsel-jq
      swiper

      # better unicode font support
      unicode-fonts

      # lsp-mode things
      lsp-mode
      lsp-haskell
      treemacs
      lsp-treemacs
      lsp-ui
      company # popups
      helm
      helm-lsp
      ivy
      lsp-ivy
      dap-mode
      which-key
    ];
  };

  home.file.".emacs.d" = {
    source = ./emacs.d;
    recursive = true;
  };
}
