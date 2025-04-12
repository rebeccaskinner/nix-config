# NB: For now, this doesn't store zim files in the nix store. Since
# they may be hundreds of gigabytes, we may want to store them on the
# same disk as the rest of the nix store. TODO: deal with this and
# keep zim files in the nix store for better reproducibility.

{ config, pkgs, lib, ... }:
let
  kiwixLocalPort = "8002";
  zimDirectory = "/var/archives/data/kiwix";
  startKiwixWithDirectory = pkgs.writeScriptBin "kiwix-server-start" ''
    #!/run/current-system/sw/bin/bash
    zimFiles=(${zimDirectory}/*.zim);
    ${pkgs.kiwix-tools}/bin/kiwix-serve -d -p ${kiwixLocalPort} ''${zimFiles[@]};
  '';

in
{
  systemd.services.kiwix-server = {
    wantedBy = ["multi-user.target"];
    after = ["network.target"];
    description = "Run a local kiwix server";
    serviceConfig = {
      Type = "forking";
      User = "kiwix";
      ExecStart = ''
        ${startKiwixWithDirectory}/bin/kiwix-server-start
      '';
    };
  };

  users.users.kiwix = {
    isSystemUser = true;
    description = "Kiwix server";
    group = "users";
  };

  services.nginx.virtualHosts."wiki.borg.cube" = {
    onlySSL = true;
    sslCertificate = "/var/www/ssl-keys/wildcard.borg.cube.crt";
    sslCertificateKey = "/var/www/ssl-keys/wildcard.borg.cube.key";
    locations."/" = {
      proxyPass = "http://127.0.0.1:${kiwixLocalPort}";
      proxyWebsockets = true;
      # extraConfig = ''
      #   add_header X-Frame-Options SAMEORIGIN always;
      # '';
    };
  };

  environment.systemPackages = with pkgs; [ kiwix-tools ];
}
