{utils, pkgs, ...}:
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
in utils.newCollection(
  with pkgs;
  [ gimp
    krita
    drawio
    blender
  ])
