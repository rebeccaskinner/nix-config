{ config, lib, pkgs, ... }:

let
  borgCubeInternal = pkgs.writeText "db.borg.cube-internal" ''
    $TTL    604800
    @       IN      SOA     ns1.borg.cube. admin.borg.cube. (
                                  6         ; Serial
                             604800         ; Refresh
                              86400         ; Retry
                            2419200         ; Expire
                             604800 )       ; Negative Cache TTL
    ;
    ; name servers - NS records
            IN      NS      ns1.borg.cube.

    ; name servers - A records
    ns1.borg.cube.          IN      A       192.168.50.148

    ; 192.168.50.0/24 A records
    fillory.borg.cube.      IN      A       192.168.50.214

    ; 192.168.50.0/24 CNAME records
    daystrom.borg.cube.     IN      CNAME   ns1.borg.cube.
    portal.borg.cube.       IN      CNAME   ns1.borg.cube.
    nextcloud.borg.cube.    IN      CNAME   ns1.borg.cube.
    photos.borg.cube.       IN      CNAME   ns1.borg.cube.
    video.borg.cube.        IN      CNAME   ns1.borg.cube.
    news.borg.cube.         IN      CNAME   ns1.borg.cube.
    audio-books.borg.cube.        IN      CNAME   ns1.borg.cube.
    www.borg.cube.          IN      CNAME   ns1.borg.cube.
    wiki.borg.cube.          IN      CNAME   ns1.borg.cube.
  '';

  reverseZoneInternal = pkgs.writeText "db.192.168.50" ''
    $TTL    604800
    @       IN      SOA     borg.cube. admin.borg.cube. (
                                  6         ; Serial
                             604800         ; Refresh
                              86400         ; Retry
                            2419200         ; Expire
                             604800 )       ; Negative Cache TTL
    ; name servers
          IN      NS      ns1.borg.cube.

    ; PTR Records
    148     IN      PTR     ns1.borg.cube.           ; 192.168.50.148
    214     IN      PTR     fillory.borg.cube.       ; 192.168.50.214
  '';

  borgCubeTailscale = pkgs.writeText "db.borg.cube-tailscale" ''
    $TTL    604800
    @       IN      SOA     ns1.borg.cube. admin.borg.cube. (
                                  6         ; Serial
                             604800         ; Refresh
                              86400         ; Retry
                            2419200         ; Expire
                             604800 )       ; Negative Cache TTL
    ;
    ; name servers - NS records
            IN      NS      ns1.borg.cube.

    ; name servers - A records
    ns1.borg.cube.          IN      A       100.123.186.116

    ; 192.168.50.0/24 A records
    fillory.borg.cube.      IN      A       100.91.57.97

    ; 192.168.50.0/24 CNAME records
    daystrom.borg.cube.     IN      CNAME   ns1.borg.cube.
    portal.borg.cube.       IN      CNAME   ns1.borg.cube.
    nextcloud.borg.cube.    IN      CNAME   ns1.borg.cube.
    photos.borg.cube.       IN      CNAME   ns1.borg.cube.
    news.borg.cube.         IN      CNAME   ns1.borg.cube.
    video.borg.cube.        IN      CNAME   ns1.borg.cube.
    audio-books.borg.cube.        IN      CNAME   ns1.borg.cube.
    www.borg.cube.          IN      CNAME   ns1.borg.cube.
    wiki.borg.cube.          IN      CNAME   ns1.borg.cube.
  '';

  reverseZoneTailscale = pkgs.writeText "db.100" ''
    $TTL    604800
    @       IN      SOA     borg.cube. admin.borg.cube. (
                                  6         ; Serial
                             604800         ; Refresh
                              86400         ; Retry
                            2419200         ; Expire
                             604800 )       ; Negative Cache TTL
    ; name servers
          IN      NS      ns1.borg.cube.

    ; PTR Records
    116.186.123.100.in-addr.arpa.     IN      PTR     ns1.borg.cube.           ; 100.123.186.116
    97.57.91.100.in-addr.arpa.     IN      PTR     fillory.borg.cube.       ; 100.91.57.97
  '';

in
{
  services.bind = {
    enable = true;
    cacheNetworks = [ "127.0.0.0/24" "192.168.50.0/24" "100.64.0.0/10" "::1/128" ];
    forwarders = [ "1.1.1.1" "1.0.0.1" "8.8.8.8" "8.8.4.4" ];
    extraConfig = ''
      view "internal" {
        match-clients { 127.0.0.0/24; ::1/128; 192.168.50.0/24; };
        recursion yes;
        zone "borg.cube" {
          type master;
          allow-transfer { };
          allow-query { 127.0.0.0/24; ::1/128; 192.168.50.0/24; };
          file "${borgCubeInternal}";
        };
        zone "192.168.50.in-addr.arpa" {
          type master;
          allow-transfer { };
          allow-query { 127.0.0.0/24; ::1/128; 192.168.50.0/24; };
          file "${reverseZoneInternal}";
        };
      };

      view "tailscale" {
        match-clients { 100.64.0.0/10; };
        recursion yes;
        zone "borg.cube" {
          type master;
          allow-transfer { };
          allow-query { 100.64.0.0/10; };
          file "${borgCubeTailscale}";
        };
        zone "100.in-addr.arpa" {
          type master;
          allow-transfer { };
          allow-query { 100.64.0.0/10; };
          file "${reverseZoneTailscale}";
        };
      };
    '';

    # zones = {
    #   "borg.cube" = {
    #     master = true;
    #     allowQuery = [ "127.0.0.0/24" "::1/128" "192.168.50.0/24" ];
    #     file = "/var/dns/zones/db.borg.cube";
    #   };

    #   "192.168.50.in-addr.arpa" = {
    #     master = true;
    #     allowQuery = [ "127.0.0.0/24" "::1/128" "192.168.50.0/24" ];
    #     file = "/var/dns/zones/db.192.168.50";
    #   };
    # };
  };
}
