{ config, lib, pkgs, ... }:

let
## borg.cube
  borgCubeInternal = pkgs.writeText "db.borg.cube-internal" ''
    $TTL    604800
    @       IN      SOA     ns1.borg.cube. admin.borg.cube. (
                         2025051201         ; Serial
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
    vtt.borg.cube.          IN      CNAME   ns1.borg.cube.
  '';

  borgCubeTailscale = pkgs.writeText "db.borg.cube-tailscale" ''
    $TTL    604800
    @       IN      SOA     ns1.borg.cube. admin.borg.cube. (
                         2025051201         ; Serial
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
    vtt.borg.cube.          IN      CNAME   ns1.borg.cube.
  '';

## internal.rebeccaskinner.net
  internalRebeccaSkinnerNetInternal = pkgs.writeText "db.internal.rebeccaskinner.net-internal" ''
    $TTL    604800
    @       IN      SOA     ns1.internal.rebeccaskinner.net. admin.internal.rebeccaskinner.net. (
                         2025051201         ; Serial
                             604800         ; Refresh
                              86400         ; Retry
                            2419200         ; Expire
                             604800 )       ; Negative Cache TTL
    ;
    ; name servers - NS records
            IN      NS      ns1.internal.rebeccaskinner.net.

    ; name servers - A records
    ns1.internal.rebeccaskinner.net.          IN      A       192.168.50.148

    ; 192.168.50.0/24 A records
    fillory.internal.rebeccaskinner.net.      IN      A       192.168.50.214

    ; 192.168.50.0/24 CNAME records
    daystrom.internal.rebeccaskinner.net.     IN      CNAME   ns1.internal.rebeccaskinner.net.
    portal.internal.rebeccaskinner.net.       IN      CNAME   ns1.internal.rebeccaskinner.net.
    nextcloud.internal.rebeccaskinner.net.    IN      CNAME   ns1.internal.rebeccaskinner.net.
    photos.internal.rebeccaskinner.net.       IN      CNAME   ns1.internal.rebeccaskinner.net.
    video.internal.rebeccaskinner.net.        IN      CNAME   ns1.internal.rebeccaskinner.net.
    news.internal.rebeccaskinner.net.         IN      CNAME   ns1.internal.rebeccaskinner.net.
    audio-books.internal.rebeccaskinner.net.        IN      CNAME   ns1.internal.rebeccaskinner.net.
    www.internal.rebeccaskinner.net.          IN      CNAME   ns1.internal.rebeccaskinner.net.
    wiki.internal.rebeccaskinner.net.          IN      CNAME   ns1.internal.rebeccaskinner.net.
    vtt.internal.rebeccaskinner.net.          IN      CNAME   ns1.internal.rebeccaskinner.net.
  '';

  internalRebeccaSkinnerNetReverseZoneInternal = pkgs.writeText "db.192.168.50-irsn" ''
    $TTL    604800
    @       IN      SOA     internal.rebeccaskinner.net. admin.internal.rebeccaskinner.net. (
                         2025051201         ; Serial
                             604800         ; Refresh
                              86400         ; Retry
                            2419200         ; Expire
                             604800 )       ; Negative Cache TTL
    ; name servers
          IN      NS      ns1.internal.rebeccaskinner.net.

    ; PTR Records
    148     IN      PTR     ns1.internal.rebeccaskinner.net.           ; 192.168.50.148
    214     IN      PTR     fillory.internal.rebeccaskinner.net.       ; 192.168.50.214
  '';

  internalRebeccaSkinnerNetTailscale = pkgs.writeText "db.internal.rebeccaskinner.net-tailscale" ''
    $TTL    604800
    @       IN      SOA     ns1.internal.rebeccaskinner.net. admin.internal.rebeccaskinner.net. (
                         2025051201         ; Serial
                             604800         ; Refresh
                              86400         ; Retry
                            2419200         ; Expire
                             604800 )       ; Negative Cache TTL
    ;
    ; name servers - NS records
            IN      NS      ns1.internal.rebeccaskinner.net.

    ; name servers - A records
    ns1.internal.rebeccaskinner.net.          IN      A       100.123.186.116

    ; 192.168.50.0/24 A records
    fillory.internal.rebeccaskinner.net.      IN      A       100.91.57.97

    ; 192.168.50.0/24 CNAME records
    daystrom.internal.rebeccaskinner.net.     IN      CNAME   ns1.internal.rebeccaskinner.net.
    portal.internal.rebeccaskinner.net.       IN      CNAME   ns1.internal.rebeccaskinner.net.
    nextcloud.internal.rebeccaskinner.net.    IN      CNAME   ns1.internal.rebeccaskinner.net.
    photos.internal.rebeccaskinner.net.       IN      CNAME   ns1.internal.rebeccaskinner.net.
    news.internal.rebeccaskinner.net.         IN      CNAME   ns1.internal.rebeccaskinner.net.
    video.internal.rebeccaskinner.net.        IN      CNAME   ns1.internal.rebeccaskinner.net.
    audio-books.internal.rebeccaskinner.net.        IN      CNAME   ns1.internal.rebeccaskinner.net.
    www.internal.rebeccaskinner.net.          IN      CNAME   ns1.internal.rebeccaskinner.net.
    wiki.internal.rebeccaskinner.net.          IN      CNAME   ns1.internal.rebeccaskinner.net.
    vtt.internal.rebeccaskinner.net.          IN      CNAME   ns1.internal.rebeccaskinner.net.
  '';

  internalRebeccaSkinnerNetReverseZoneTailscale = pkgs.writeText "db.100-irsn" ''
    $TTL    604800
    @       IN      SOA     internal.rebeccaskinner.net. admin.internal.rebeccaskinner.net. (
                         2025051201         ; Serial
                             604800         ; Refresh
                              86400         ; Retry
                            2419200         ; Expire
                             604800 )       ; Negative Cache TTL
    ; name servers
          IN      NS      ns1.internal.rebeccaskinner.net.

    ; PTR Records
    116.186.123.100.in-addr.arpa.     IN      PTR     ns1.internal.rebeccaskinner.net.           ; 100.123.186.116
    97.57.91.100.in-addr.arpa.     IN      PTR     fillory.internal.rebeccaskinner.net.       ; 100.91.57.97
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

        zone "internal.rebeccaskinner.net" {
          type master;
          allow-transfer { };
          allow-query { 127.0.0.0/24; ::1/128; 192.168.50.0/24; };
          file "${internalRebeccaSkinnerNetInternal}";
        };

        zone "192.168.50.in-addr.arpa" {
          type master;
          allow-transfer { };
          allow-query { 127.0.0.0/24; ::1/128; 192.168.50.0/24; };
          file "${internalRebeccaSkinnerNetReverseZoneInternal}";
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
        zone "internal.rebeccaskinner.net" {
          type master;
          allow-transfer { };
          allow-query { 100.64.0.0/10; };
          file "${internalRebeccaSkinnerNetTailscale}";
        };
        zone "100.in-addr.arpa" {
          type master;
          allow-transfer { };
          allow-query { 100.64.0.0/10; };
          file "${internalRebeccaSkinnerNetReverseZoneTailscale}";
        };
      };
    '';
  };
}
