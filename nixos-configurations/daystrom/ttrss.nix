{ pkgs, config, lib, ... }:
{
  services.tt-rss = {
    enable = true;
    virtualHost = "news.borg.cube";
    selfUrlPath = "https://news.borg.cube";
    registration.enable = false;
    user = "tt_rss";
    pluginPackages = with pkgs; [
      tt-rss-plugin-feediron
    ];
  };

  services.nginx.virtualHosts."news.borg.cube" = {
    addSSL = true;
    sslCertificate = "/var/www/ssl-keys/wildcard.borg.cube.crt";
    sslCertificateKey = "/var/www/ssl-keys/wildcard.borg.cube.key";
    locations."/" = {
      proxyPass = "http://127.0.0.1";
      proxyWebsockets = true;
    };
  };

  # services.postgresql = {
  #   enable = true;
  #   ensureDatabases = ["nextcloud"];
  #   ensureUsers = [
  #     { name = "nextcloud";
  #       # ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
  #       ensureDBOwnership = true;
  #     }];
  # };

  # services.nginx.virtualHosts.${config.services.nextcloud.hostName} = {
  #   onlySSL = true;
  #   sslCertificate = "/var/www/ssl-keys/borg.cube.crt";
  #   sslCertificateKey = "/var/www/ssl-keys/borg.cube.key";
  # };

  # systemd.services."nextcloud-setup" = {
  #   requires = ["postgresql.service"];
  #   after = ["postgresql.service"];
  # };

}
