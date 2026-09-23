{ pkgs
, utils
, extraPackages ? (epkgs: [])
, extraConfigs ? []
, emacsPackage ? pkgs.emacs
, createMacosSymlink ? false
, ...}:

let
  userExtraPackages = extraPackages;
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
    version = "0.3";
    src = pkgs.fetchFromGitHub {
      owner = "rebeccaskinner";
      repo = "darkplum-theme";
      rev = "7a290bbb0dae2839825dd51e27ca4907ff06d529";
      sha256 = "0smjim1mhh9n5vs9v5rq09pnj68vwf0ykn3dgwcgygxkd8wv52lx";
    };
    meta = {
      description = "A dark purple theme for emacs";
      longDescription = "A dark purple theme for emacs";
    };
    license = pkgs.lib.licenses.gpl3Plus.spdxId;
  };

  pml-mode = import ./packages/pml-mode { emacs = emacsPackage; lib = pkgs.lib; };

  emacsFiles = emacsConfigDir // emacsAppLink;
in
utils.env.importOnlyEnvironment ({
  services.emacs = {
    enable = true;
    startWithUserSession = true;
  };
  systemd.user.services.emacs.Service = {
    EnvironmentFile = ["%h/.config/secrets/emacs-llm.env"];
  };
  programs.emacs = {
    enable = true;
    package = emacsPackage;

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
            pdf-tools
            proof-general
            magit
            ox-gfm
            restclient
            format-sql
            paredit
            # ox-pandoc
            graphviz-dot-mode
            go-playground
            expand-region
            rainbow-delimiters
            sql-indent
            pdf-tools
            goto-last-change
            darkplum-theme
            dracula-theme
            inkpot-theme
            fzf
            vterm

            # completion: minibuffer
            vertico
            orderless
            marginalia
            consult
            embark
            embark-consult
            # completion: in-buffer
            corfu
            corfu-terminal
            cape

            # LLMs
            org-ai
            gptel
            plz
            transient

            # evil
            evil
            evil-collection
            evil-org

            # markdown (also pulled in by pml-mode, but init.el configures it directly)
            markdown-mode
            edit-indirect   # markdown-edit-code-block (C-c ')

            # Git / Github integration
            forge
            github-review

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
            dap-mode
            which-key

            # additional modes
            pml-mode
          ];
      in defaults ++ (userExtraPackages epkgs);
  };
  home.file = emacsFiles;
})
