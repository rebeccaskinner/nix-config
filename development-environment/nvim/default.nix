{ pkgs
, utils
, extraPackages ? []
}:
utils.env.importOnlyEnvironment ({
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    plugins = with pkgs.vimPlugins; [
      NrrwRgn
      aurora
      b64-nvim
      bat-vim
      codi-vim
      csv
      dhall-vim
      diffview-nvim
      dirbuf-nvim
      direnv-vim
      dracula-nvim
      easy-align
      editorconfig-nvim
      file-line
      fzf-checkout-vim
      fzf-hoogle-vim
      fzf-lua
      fzf-vim
      fzfWrapper
      galaxyline-nvim
      ghcid
      git-blame-nvim
      gitlinker-nvim
      graphviz-vim
      haskell-vim
      hoogle
      html5-vim
      hydra-nvim
      markdown-preview-nvim
      nvim-fzf
      nvim-fzf-commands
      nvim-gdb
      nvim-highlight-colors
      nvim-hs-vim
      nvim-lint
      nvim-surround
      nvim-treesitter
      nvim-treesitter-context
      nvim-treesitter-pyfold
      nvim-treesitter-refactor
      nvim-treesitter-textobjects
      nvim-ts-autotag
      nvim-ts-context-commentstring
      nvim-ts-rainbow2
      orgmode
      pgsql-vim
      popup-nvim
      prettyprint
      prev_indent
      python-mode
      readline-vim
      rust-vim
      telescope-asynctasks-nvim
      telescope-file-browser-nvim
      telescope-fzf-native-nvim
      telescope-live-grep-args-nvim
      telescope-nvim
      telescope-project-nvim
      telescope-symbols-nvim
      telescope-vim-bookmarks-nvim
      telescope_hoogle
      ultisnips
      unicode-vim
      vim-nix
      rose-pine
      kanagawa-nvim
      nightfox-nvim
    ];
    extraConfig = builtins.readFile ./nvim-init.vim;
    extraLuaConfig = builtins.readFile ./nvim-init.lua;
  };
})
