# Emacs, run as a user daemon, with the configuration in ../emacs/emacs.d and
# the set of packages that configuration expects.
#
# Language-specific emacs configuration used to be assembled from the
# development profiles; that is no longer done. The major modes for the
# languages in profiles/development are simply listed here.
{ pkgs, primaryUser, inputs, ... }:
let
  emacsPackage = pkgs.emacs;

  darkplum-theme = emacsPackage.pkgs.melpaBuild {
    pname = "darkplum-theme";
    version = "0.3";
    src = inputs.darkplum-theme;
    meta.description = "A dark purple theme for emacs";
  };

  pml-mode = import ../emacs/packages/pml-mode {
    emacs = emacsPackage;
    lib = pkgs.lib;
  };

  persistent-mode = import ../emacs/packages/persistent-mode {
    emacs = emacsPackage;
    lib = pkgs.lib;
  };

  emacsPackages = epkgs: with epkgs; [
    # editing
    ace-window
    evil
    evil-collection
    evil-org
    expand-region
    goto-last-change
    paredit
    rainbow-delimiters
    fill-column-indicator
    which-key
    bug-hunter

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

    # tools
    direnv
    flycheck
    fzf
    magit
    forge
    github-review
    restclient
    vterm
    pdf-tools
    go-playground

    # LLMs
    gptel
    org-ai
    plz
    transient

    # lsp
    lsp-mode
    lsp-ui
    lsp-treemacs
    treemacs
    dap-mode

    # themes
    darkplum-theme
    dracula-theme
    inkpot-theme

    # documents and org
    markdown-mode
    edit-indirect   # markdown-edit-code-block (C-c ') edits a fenced block in its own mode
    ox-gfm
    persistent-mode
    pml-mode

    # languages
    cargo
    dhall-mode
    format-sql
    go-mode
    graphviz-dot-mode
    haskell-mode
    json-mode
    nix-mode
    nixfmt
    proof-general
    protobuf-mode
    rustic
    sql-indent
    yaml-mode
  ];
in
{
  home-manager.users.${primaryUser} = {
    programs.emacs = {
      enable = true;
      package = emacsPackage;
      extraPackages = emacsPackages;
    };

    services.emacs = {
      enable = true;
      startWithUserSession = true;
    };

    # API keys for gptel / org-ai. The leading "-" makes the file optional so
    # the daemon still starts on a machine without secrets configured.
    systemd.user.services.emacs.Service.EnvironmentFile =
      [ "-%h/.config/secrets/emacs-llm.env" ];

    home.file.".emacs.d" = {
      source = ../emacs/emacs.d;
      recursive = true;
    };

    home.sessionVariables.EDITOR = "emacs";
  };
}
