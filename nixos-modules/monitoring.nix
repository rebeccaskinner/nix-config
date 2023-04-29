{ pkgs, config, lib, ... }:
{
  services.grafana = {
    enable = true;
    domain = "grafana.borg.cube";
    port = 8090;
    addr = "127.0.0.1";
  };

  services.nginx.virtualHosts.${config.services.grafana.domain} = {
    onlySSL = true;
    sslCertificate = "/var/www/ssl-keys/infra.borg.cube.crt";
    sslCertificateKey = "/var/www/ssl-keys/infra.borg.cube.key";
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}/";
      proxyWebsockets = true;
    };
  };
  services.prometheus = {
    enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
      };
    };
  };
}
