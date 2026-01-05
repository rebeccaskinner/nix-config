{ pkgs
, utils
, ...}:
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
