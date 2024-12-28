{ pkgs
, utils
, ...}:
utils.env.importOnlyEnvironment ({
  programs.tmux = {
    enable = true;
    mouse = true;
    clock24 = true;
    plugins = with pkgs.tmuxPlugins; [sensible];
    terminal = "tmux-256color";
    extraConfig = ''
      set -g status-bg "#2a2035"
      set -g status-fg "#dda0dd"
      set -gu default-command
      set -g default-shell "$SHELL"
    '';
  };
})
