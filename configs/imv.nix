{ pkgs
, utils
, ...}:
{
  programs.imv = {
    enable = true;
  };
  xdg.mimeApps.defaultApplications = {
    "image/png"  = "imv.desktop";
    "image/jpeg" = "imv.desktop";
    "image/webp" = "imv.desktop";
    "image/gif"  = "imv.desktop";
    "image/svg+xml" = "imv.desktop";
  };
}
