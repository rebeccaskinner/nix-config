{ pkgs, ...}:
{
  home.pointerCursor = {
    enable = true;
    x11.enable = true;
    x11.defaultCursor = "left_ptr";
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
  };
}
