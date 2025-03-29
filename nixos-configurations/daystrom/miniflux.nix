{ pkgs, config, lib, ... }:
let
  http-port = "8181";
in
{
  environment.etc."miniflux-admin-credentials" = {
    text = ''
      ADMIN_USERNAME=rebecca
      ADMIN_PASSWORD=changeMe!0000
    '';
    mode = "0600";
    user = "miniflux";
    group = "miniflux";
  };
  services.miniflux = {
    enable = true;
    createDatabaseLocally = true;
    adminCredentialsFile = "/etc/miniflux-admin-credentials";
    config = {
      BASE_URL = "https://news.borg.cube/";
      PORT = http-port;
    };
  };
  services.nginx.virtualHosts."news.borg.cube" = {
    onlySSL = true;
    sslCertificate = "/var/www/ssl-keys/wildcard.borg.cube.crt";
    sslCertificateKey = "/var/www/ssl-keys/wildcard.borg.cube.key";
    locations."/" = {
      proxyPass = "http://127.0.0.1:${http-port}";
      proxyWebsockets = true;
    };
  };

}
