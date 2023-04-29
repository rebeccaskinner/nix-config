{utils, ...}:
let
  darkplum-settings = {
      background = "#2a2035";
      foreground = "#dda0dd";
      background_opacity = "0.9";
      remember_window_size = "yes";
  };
  dracula-settings = {
    # https://draculatheme.com/kitty
    #
    # Installation instructions:
    #
    #  cp dracula.conf ~/.config/kitty/
    #  echo "include dracula.conf" >> ~/.config/kitty/kitty.conf
    #
    # Then reload kitty for the config to take affect.
    # Alternatively copy paste below directly into kitty.conf

    foreground = "#f8f8f2";
    # background = "#282a36";
    background = "#2a2035";
    selection_foreground = "#ffffff";
    selection_background = "#44475a";

    url_color = "#8be9fd";

    # black
    color0  = "#21222c";
    color8  = "#6272a4";

    # red
    color1  = "#ff5555";
    color9  = "#ff6e6e";

    # green
    color2  = "#50fa7b";
    color10 = "#69ff94";

    # yellow
    color3  = "#f1fa8c";
    color11 = "#ffffa5";

    # blue
    color4  = "#bd93f9";
    color12 = "#d6acff";

    # magenta
    color5  = "#ff79c6";
    color13 = "#ff92df";

    # cyan
    color6  = "#8be9fd";
    color14 = "#a4ffff";

    # white
    color7  = "#f8f8f2";
    color15 = "#ffffff";

    # Cursor colors
    cursor            = "#f8f8f2";
    cursor_text_color = "background";

    # Tab bar colors
    active_tab_foreground   = "#282a36";
    active_tab_background   = "#f8f8f2";
    inactive_tab_foreground = "#282a36";
    inactive_tab_background = "#6272a4";

    # Marks
    mark1_foreground = "#282a36";
    mark1_background = "#ff5555";

    # Splits/Windows
    active_border_color = "#f8f8f2";
    inactive_border_color = "#6272a4";
  };
  darkside-settings = {
    background = "#212324";
    foreground = "#b9b9b9";
    cursor = "#bbbbbb";
    selection_background = "#2f3333";
    color0 = "#000000";
    color8 = "#000000";
    color1 = "#e8331c";
    color9 = "#df5a4f";
    color2 = "#68c156";
    color10 = "#76b768";
    color3 = "#f1d32b";
    color11 = "#eed64a";
    color4 = "#1c98e8";
    color12 = "#387bd2";
    color5 = "#8e69c8";
    color13 = "#957bbd";
    color6 = "#1c98e8";
    color14 = "#3d96e2";
    color7 = "#b9b9b9";
    color15 = "#b9b9b9";
    selection_foreground = "#212324";
  };
  opacity-settings = {
    background_opacity = "0.9";
  };
  other-settings = {
    remember_window_size = "yes";
  };
in utils.env.configOnlyEnvironment ({
  programs.kitty = {
    enable = true;
    font.name = "Fira Code";
    settings = dracula-settings // opacity-settings // other-settings;
  };
})
