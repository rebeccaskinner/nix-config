{ config, lib, pkgs, ... }:
{
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vpl-gpu-rt
      intel-compute-runtime
    ];
  };

  users.users.jellyfin.extraGroups = [ "video" "render" ];

  systemd.services.jellyfin.serviceConfig.Environment = [
    "LIBVA_DRIVER_NAME=iHD"
  ];

  environment.systemPackages = with pkgs; [ ffmpeg ];

  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  services.nginx.virtualHosts."video.borg.cube" = {
    addSSL = true;
    sslCertificate = "/var/www/ssl-keys/wildcard.borg.cube.crt";
    sslCertificateKey = "/var/www/ssl-keys/wildcard.borg.cube.key";
    locations."/" = {
      proxyPass = "http://127.0.0.1:8096";
      proxyWebsockets = true;
      extraConfig = ''
        add_header Strict-Transport-Security $hsts_header always;
        add_header Referrer-Policy "origin-when-cross-origin" always;
        add_header X-Content-Type-Options "nosniff" always;
        add_header X-Frame-Options "SAMEORIGIN" always;  # safer than DENY if you ever embed locally
        proxy_buffering off;
      '';
    };
  };
}
