{ pkgs, config, lib, ... }:
{
  services.postgresql = {
    enable = true;
    ensureDatabases = [];
    ensureUsers = [];
  };
}
