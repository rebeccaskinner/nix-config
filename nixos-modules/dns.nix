{ config, lib, pkgs, ... }:

{
  services.bind = {
    enable = true;
    cacheNetworks = [ "127.0.0.0/24" "192.168.50.0/24" ];
    forwarders = [ "1.1.1.1" "1.0.0.1" "8.8.8.8" "8.8.4.4" ];
    zones = {
      "borg.cube" = {
        master = true;
        file = "/var/dns/zones/db.borg.cube";
      };
      "192.168.50.in-addr.arpa" = {
        master = true;
        file = "/var/dns/zones/db.192.168.50";
      };
    };
  };
}
