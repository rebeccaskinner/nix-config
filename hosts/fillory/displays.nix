# Display settings for fillory: a single 4K monitor on the nvidia driver.
#
# Without an explicit DPI the nvidia driver derives one from the monitor's
# EDID (about 160 here), which Qt and Xft honour while GTK3 ignores it and
# assumes 96. Pin both to the same value so toolkits agree. `services.xserver.dpi`
# is what the X server reports; `Xft.dpi` is what GTK3 actually reads.
{ primaryUser, ... }:
let
  dpi = 96;
in
{
  services.xserver.dpi = dpi;

  home-manager.users.${primaryUser} = {
    xresources.properties."Xft.dpi" = dpi;
  };
}
