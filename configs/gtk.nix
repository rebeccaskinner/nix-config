{pkgs, ...}:
let
  gtkDarkTheme =
    { gtk-application-prefer-dark-theme = true;
    };

in
{
  gtk = {
    enable = true;
    gtk4.extraConfig = gtkDarkTheme;
    gtk3.extraConfig = gtkDarkTheme;
    iconTheme = {
      package = pkgs.beauty-line-icon-theme;
      name = "elementary";
    };
  };
}
