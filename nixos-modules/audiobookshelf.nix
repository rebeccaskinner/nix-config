{ config, pkgs, lib, ... }:
let
  audiobookshelfPort = 8005;
  audiobookshelfBindAddress = "127.0.0.1";
  audiobookDirectory = "/var/archive/data/audio-books";
in
{
  users = {
    users.audiobookshelf.isSystemUser = true;
    groups.audiobookshelf.members = ["audiobookshelf"];
  };

  systemd.services.audiobookshelf = with pkgs; {
    enable = true;
    description = "self-hosted audiobook server";
    serviceConfig = {
      Type = "simple";
      WorkingDirectory = audiobookDirectory;
      ExecStart = "${audiobookshelf}/bin/audiobookshelf --host 127.0.0.1 --port ${audiobookshelfPort}";
      ExecReload = "${util-linux}/bin/kill -HUP $MAINPID";
      Restart = "always";
      User = "audiobookshelf";
      Group = "audiobookshelf";
    };
    wantedBy = ["multi-user.target"];
    requires = ["network.target"];
  };

  services.nginx.virtualHosts."wiki.borg.cube" = {
    onlySSL = true;
    sslCertificate = "/var/www/ssl-keys/borg.cube.crt";
    sslCertificateKey = "/var/www/ssl-keys/borg.cube.key";
    locations."/" = {
      proxyPass = "http://127.0.0.1:${audiobookshelfPort}";
      proxyWebsockets = true;
      # extraConfig = ''
      #   add_header X-Frame-Options SAMEORIGIN always;
      # '';
    };
  };

}
