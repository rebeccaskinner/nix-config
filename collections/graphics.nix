{utils, pkgs, ...}:

utils.env.packagesEnvironment (
  with pkgs;
  [ gimp
    krita
    drawio
    # blender
    inkscape
    scrot
  ])
