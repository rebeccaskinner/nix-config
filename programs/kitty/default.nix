{config, pkgs, ...}:
{
  programs.kitty = {
    enable = true;
    font.name = "Fira Code";
    settings = {
      background = "#2a2035";
      foreground = "#dda0dd";
      background_opacity = "0.9";
      remember_window_size = "yes";
    };
    # extraConfig = ''
    #   background #2a2035
    #   foreground #dda0dd
    # '';
  };
}
