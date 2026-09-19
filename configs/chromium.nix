{ pkgs, ... }:
{
  programs.chromium = {
    enable = true;
    extensions = [
      { # ublock origin lite
        id = "ddkjiahejlhfcafbddmgiahcphecmpfh";
      }
    ];
  };
}
