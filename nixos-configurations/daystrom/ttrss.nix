{ pkgs, config, lib, ... }:

{
  services.tt-rss = {
    virtualHost = "news.borg.cube";
    selfUrlPath = "https://news.borg.cube";
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

