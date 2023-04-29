{ pkgs, config, lib, ... }:

{
  services.nextcloud = {
    enable = true;
    hostName = "nextcloud.borg.cube";
    package = pkgs.nextcloud26;
    extraApps = with pkgs.nextcloud26Packages.apps; { inherit calendar contacts deck notes news groupfolders tasks bookmarks; };
    extraAppsEnable = true;
    https = true;
    config = {
      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql";
      dbname = "nextcloud";
      adminpassFile = "/var/nextcloud/admin_passfile";
      adminuser = "root";
    };
    extraOptions = {
      redis = {
        host = "/run/redis-nextcloud/redis.sock";
        port = 0;
      };
      memcache = {
        local = "\\OC\\Memcache\\Redis";
        distributed = "\\OC\\Memcache\\Redis";
        locking = "\\OC\\Memcache\\Redis";
      };
    };
  };

  services.redis.servers.nextcloud = {
    enable = true;
    user = "nextcloud";
    port = 0;
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = ["nextcloud"];
    ensureUsers = [
      { name = "nextcloud";
        ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
      }];
  };

  services.nginx.virtualHosts.${config.services.nextcloud.hostName} = {
    onlySSL = true;
    sslCertificate = "/var/www/ssl-keys/borg.cube.crt";
    sslCertificateKey = "/var/www/ssl-keys/borg.cube.key";
  };

  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };

}
