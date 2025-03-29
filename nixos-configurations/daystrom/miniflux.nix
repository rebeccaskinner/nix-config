{ pkgs, config, lib, http-port ? "8181", ... }:
{
  services.miniflux = {
    enable = true;
    database.createLocally = true;
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
