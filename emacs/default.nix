{ pkgs
, utils
, userDefinedPackages ? ({...}: [])
, extraConfigs ? []
, emacsPackage ? pkgs.emacs
, createMacosSymlink ? false
, ...}:

let
  emacsAppLink =
    if createMacosSymlink
    then
      { "emacs.app" = {
          source = "${emacsPackage}/Applications/Emacs.app";
        };
      }
    else
      {
      };

  emacsConfigDir = {
    ".emacs.d" = {
      source = ./emacs.d;
      recursive = true;
    };
  };

  darkplum-theme = emacsPackage.pkgs.melpaBuild {
    name = "darkplum-theme";
    pname = "darkplum-theme";
    version = "0.0.2";
    src = pkgs.fetchFromGitHub {
      owner = "rebeccaskinner";
      repo = "darkplum-theme";
      rev = "7a93038ede49c1c30e540d510d54ef83e00c2bce";
      sha256 = "0rsfvx8nsq9sbkdzs4mjqfwsqdwg5zz9y4wyqq05pamdhchmy2al";
    };
    meta = {
      description = "A dark purple theme for emacs";
      longDescription = "A dark purple theme for emacs";
    };
    license = pkgs.lib.licenses.gpl3Plus.spdxId;
  };


  emacsFiles = emacsConfigDir // emacsAppLink;
in
utils.env.importOnlyEnvironment ({
  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    package = emacsPackage;
    overrides = self: super: rec {
          };

    extraConfig =
      builtins.foldl' (a: b: a + b) "" extraConfigs;

    extraPackages = epkgs:
      let
        defaults = with epkgs;
          [ ace-window
            bug-hunter
            flycheck
            direnv
            fill-column-indicator
            auto-complete
            pdf-tools
            proof-general
            magit
            ox-gfm
            restclient
            format-sql
            paredit
            ox-pandoc
            graphviz-dot-mode
            go-playground
            expand-region
            rainbow-delimiters
            sql-indent
            use-package
            pdf-tools
            goto-last-change
            darkplum-theme
            dracula-theme
            inkpot-theme
            counsel
            counsel-org-clock
            counsel-tramp
            counsel-etags
            counsel-jq
            swiper
            fzf
            vterm

            # evil
            evil

            # Git / Github integration
            forge
            github-review

            # tree-sitter
            tree-sitter
            tree-sitter-indent
            tree-sitter-langs

            # misc. programming language modes
            dhall-mode
            protobuf-mode
            go-mode
            json-mode
            yaml-mode
            nix-mode
            nix-haskell-mode
            nixfmt

            # lsp-mode things
            lsp-mode
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
        extras = userDefinedPackages epkgs;
      in defaults ++ extras;
  };
  home.file = emacsFiles;
})
