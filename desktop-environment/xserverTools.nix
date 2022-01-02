{pkgs
,...
}:

let
  xtools = with pkgs; [
    xorg.xcursorthemes
    hicolor-icon-theme
    breeze-gtk
  ];
in { home.packages = xtools; }
