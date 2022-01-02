let
  fontImport = {pkgs, ...}: {
    home.packages =
      with pkgs; [
        font-awesome-ttf
        siji
        material-design-icons
        hasklig
        font-awesome
        symbola ];
  };
in [fontImport]
