{config, pkgs, ...}:
{
  programs.kitty = {
    enable = true;
    font.name = "Fira Code";
    extraConfig = ''
      background #2a2035
      foreground #dda0dd
    '';
  };
}
