{config, pkgs, ...}:
{
  services.screen-locker = {
    enable = true;
    inactiveInterval = 10;
    xautolock = {
      enable = true;
    };
    lockCmd =
  };
}
