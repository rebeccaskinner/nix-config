{ pkgs
, utils
, extraPackages ? []
}:
utils.env.importOnlyEnvironment ({
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      dracula-theme.theme-dracula
      file-icons.file-icons
      haskell.haskell
    ];
  };
})
