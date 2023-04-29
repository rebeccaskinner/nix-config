{ config, lib, pkgs, ... }:
{
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-compute-runtime
    ];
  };

  environment.systemPackages = with pkgs; [ ffmpeg ];

  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  services.nginx.virtualHosts."video.borg.cube" = {
    addSSL = true;
    sslCertificate = "/var/www/ssl-keys/borg.cube.crt";
    sslCertificateKey = "/var/www/ssl-keys/borg.cube.key";
    locations."/" = {
      proxyPass = "http://127.0.0.1:8096";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_buffering off;
      '';
    };
  };
}
