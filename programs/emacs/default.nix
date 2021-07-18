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
          rev = "c4a3d472775f1d534ce4e23a226a3b2b462f624d";
          sha256 = "0wlv86dfsv6vl3ik6xygdaxd9j1iy5kwibsy5csr7isry26w8kbj";
          # rev = "a15b576f7c3886962dc0f05a6607377969545ae4";
          # sha256 = "0jx57cx7n2k2a8ysbqlamc8m70vg9crajc25axbd75cn34ccdr4i";
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
    ];
  };

  home.file.".emacs.d" = {
    source = ./emacs.d;
    recursive = true;
  };
}
