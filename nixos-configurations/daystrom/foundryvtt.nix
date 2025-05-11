{ pkgs, inputs, config, lib, ... }:
let
  http-port = "8123";
  https-port = 443;
in
{

  # NB: Need to add foundry versions to the nix store manually
  # $ nix-store --add-fixed sha256 FoundryVTT-<version>.zip
  # /nix/store/<hash>-FoundryVTT-<version>.zip
  # $ mkdir -p <some path>
  # $ nix-store --add-root <some path>/FoundryVTT-<version>.zip -r /nix/store/<hash>-FoundryVTT-<version>.zip
  # <some path>/FoundryVTT-<version>.zip
  # $ ls -al <some path>
  # total 0
  # drwxr-xr-x   3 reckenrode staff   96 Jun 18 18:33 ./
  # drwxr-x---+ 75 reckenrode staff 2400 Jun 18 18:33 ../
  # lrwxr-xr-x   1 reckenrode staff   65 Jun 18 18:33 FoundryVTT-<version>.zip -> /nix/store/<hash>-FoundryVTT-<version>.zip

  services.foundryvtt = {
    enable = true;
    hostname = "vtt.borg.cube";
    minifyStaticFiles = true;
    proxySSL = true;
    proxyPort = https-port;
    upnp = false;
    package = inputs.foundryvtt.packages.${pkgs.system}.foundryvtt_13;
  };

  services.nginx.virtualHosts."vtt.borg.cube" = {
    onlySSL = true;
    sslCertificate = "/var/www/ssl-keys/wildcard.borg.cube.crt";
    sslCertificateKey = "/var/www/ssl-keys/wildcard.borg.cube.key";
    locations."/" = {
      proxyPass = "http://127.0.0.1:${http-port}";
      proxyWebsockets = true;
    };
  };
}
