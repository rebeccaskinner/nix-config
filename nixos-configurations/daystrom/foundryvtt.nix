{ pkgs, inputs, config, lib, ... }:
let
  http-port = "30000";
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
    hostName = "vtt.internal.rebeccaskinner.net";
    minifyStaticFiles = true;
    proxySSL = true;
    proxyPort = https-port;
    port = 30000;
    upnp = false;
    package = inputs.foundryvtt.packages.${pkgs.system}.foundryvtt_13.overrideAttrs {
      version = "13.0.0+342";
    };
  };

  services.nginx.virtualHosts."vtt.internal.rebeccaskinner.net" = {
    onlySSL = true;
    sslCertificate = "/var/www/ssl-keys/wildcard.internal.rebeccaskinner.net.crt";
    sslCertificateKey = "/var/www/ssl-keys/wildcard.internal.rebeccaskinner.net.key";
    extraConfig = ''
      client_max_body_size 300M;
    '';

    locations."/" = {
      proxyPass = "http://localhost:30000";
      extraConfig = ''
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "Upgrade";
        proxy_pass_request_headers on;
        proxy_cookie_path / "/; SameSite=Lax; Secure";
      '';
    };
       
  };
}
