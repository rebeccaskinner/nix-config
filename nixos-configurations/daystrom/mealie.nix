{ pkgs, config, lib, ... }:
let
  hostname = "recipes.borg.cube";
  mealiePort = 8183;
in
{
  services.mealie = {
    enable = true;
    database.createLocally = true;
    listenAddress = "127.0.0.1";
    port = mealiePort;

    settings = {
      BASE_URL = "https://${hostname}";
      TZ = config.time.timeZone;
      ALLOW_SIGNUP = "false";
      API_DOCS = "false";
    };
  };

  services.nginx.virtualHosts.${hostname} = {
    forceSSL = true;
    sslCertificate = "/var/www/ssl-keys/wildcard.borg.cube.crt";
    sslCertificateKey = "/var/www/ssl-keys/wildcard.borg.cube.key";
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString mealiePort}";
      proxyWebsockets = true;
      recommendedProxySettings = true;
      extraConfig = ''
        client_max_body_size 500M;
        proxy_read_timeout   600s;
        proxy_send_timeout   600s;
        send_timeout         600s;
      '';
    };
  };
}
