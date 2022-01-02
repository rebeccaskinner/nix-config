let
  graphicsTools = {pkgs, ...}: {
    home.packages =
      with pkgs; [
        gimp
        krita
        drawio
        blender
      ];
  };
in [graphicsTools]
