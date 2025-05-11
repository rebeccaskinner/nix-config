# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ inputs }:
{ config, pkgs, ... }:
{
  # nixpkgs.config.allowUnfree = true;
  nix.settings.experimental-features = ["nix-command" "flakes"];

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # temporarily broken, using security.pki.certificates instead
  # security.pki.certificateFiles = ["/var/certs/borg.cube.crt"];

  security.pki.certificates = [
    # borg.cube.crt
''
-----BEGIN CERTIFICATE-----
MIIFsjCCA5qgAwIBAgIULKseXC11/QX2Yn43njCir8qHbRcwDQYJKoZIhvcNAQEL
BQAwFDESMBAGA1UEAwwJYm9yZy5jdWJlMB4XDTIzMDQyMzA0MjgyNVoXDTMzMDQy
MDA0MjgyNVowFDESMBAGA1UEAwwJYm9yZy5jdWJlMIICIjANBgkqhkiG9w0BAQEF
AAOCAg8AMIICCgKCAgEA4RuHjWEFOw75l5d01O5L3asJEEBAOEYhdV8j8ByKQsKj
tYU0ugZt1d8AB1TmrDF9/yhEA48XmxehHgyQe3UmSejiHwGpbTvx5D5gcORDG4yd
JxWv1OxYwktDHENJF1Owyv8sBqEZ3FhoYz3wVX1ttcOsuumjS5KdRlQI2GYjl9IV
OYyrwM1yMVrj37Wxk4cp5vwkhd8tUehXlsDFUyd0ZILG1wCdO47UA64LIvkmLi/D
Yggo5oy3Bdls98PrrNTcw0iBR7mqPQTwA1++1WUa3xyGkR9Q70szLmALv9W7XLzv
wAnwJVeBPtxJ1GLC9JHN7ItKz7AqGS+XjEyY3j98ynB0qV9QhMhMF1Q1EwREB9yn
tN2yjWWfQ2J6HEZ/VbTq8FydaR/m0Gmt2wlc3pLEiaxxhpcDyIqo5U7wWqYb8myZ
WXNATPxGed+zE7oR1wkuvQ5XTpJ7iif/uxhs3Bp3lI0PEk7FQ0Cz5muISC/L+CId
MDIO5B5HVpisAnww5r7n4snAaF7T1jq4ti8GEqgm/x+F4Sd3tmJ25Lv0ladKr+Ye
6CQdzFHGtg3Ysh57uF4nzEedHVg9uCO4oFm40qPVrRm9z3c7xclrn/Qb+zKmQLlN
811ml4FbpjX6qSA2Q0MvdcxChvQ675JKDsNs932IhJTX9IH6PSfDTk0XnLm5ReMC
AwEAAaOB+zCB+DAdBgNVHQ4EFgQUnWfG2C1tfLFB4+BnQ8yIgADTWm4wHwYDVR0j
BBgwFoAUnWfG2C1tfLFB4+BnQ8yIgADTWm4wDwYDVR0TAQH/BAUwAwEB/zCBpAYD
VR0RBIGcMIGZgglib3JnLmN1YmWCDXd3dy5ib3JnLmN1YmWCEHBvcnRhbC5ib3Jn
LmN1YmWCD3ZpZGVvLmJvcmcuY3ViZYIOd2lraS5ib3JnLmN1YmWCE25leHRjbG91
ZC5ib3JnLmN1YmWCEGhvb2dsZS5ib3JnLmN1YmWCEWhhY2thZ2UuYm9yZy5jdWJl
ghBwaG90b3MuYm9yZy5jdWJlMA0GCSqGSIb3DQEBCwUAA4ICAQA+2L8derS8WPfD
+SJBcZ7eCadVXwIBxL/qGvtQ3any/eZIwVeW5xLNJEF1fnVoLxvVdcgClvHxF8eV
u1xNyUrtBKvx2UFr3or2e9NKxN2bskapFFqrfhO+0eKW8482tvvful0NIGyipwhQ
aX/4842dEZpjM+OddReVPtbTSz58sDl0k5bjh3MX6trjXSIlimiY7GPhMFqcdHqS
ft3766phky9XCKJER19F/nBolKaGzRT+UdvaVp30D7o3qiuLWxMT7Bwig8y9yliw
mZFkBLBvdQEDjutO//DJ0Q2lssmDyLoiNStg722f19rcq6L/PgBKlJhjczsKpdUs
3sRUYKrBFKIkXjKGZVZk8MjmS0fo603bkxw6Cloxu1F3kJV0uA7570Lv9fmY4qu7
FH5PCBbcipxQ2pcFEqT1QAISPEEiG4RhrqtFyeTRJgK5ZrEPIM65prAB1396K5Mq
1y8GK+qV8xj1Gnc6ciQhqcvHM6t9zgqGbyj/csDqPp90wRzhf+9INrVZf8ZJQ+qv
PB9G5UTZBDXCk9e1/gx0NTGCi5PAnROJBEBlZm/ygOMFM99GUYMQDaccZNR+PuYN
bZTcjwGEi1bLZPrOGDFHYyljwYJQluC/ZZF5fbTfJjb8m/OgbKvBa0Kh3PE2nkfs
+7q3HwSRzwwi/oXc39YFl+1eLa9e4A==
-----END CERTIFICATE-----
''
# wildcard.borg.cube.crt
''
-----BEGIN CERTIFICATE-----
MIIFMDCCAxigAwIBAgIUWy8aoAgyuykPL+dueS70VOyXZPIwDQYJKoZIhvcNAQEL
BQAwFjEUMBIGA1UEAwwLKi5ib3JnLmN1YmUwHhcNMjUwMzIzMjEzMjMzWhcNMzUw
MzIxMjEzMjMzWjAWMRQwEgYDVQQDDAsqLmJvcmcuY3ViZTCCAiIwDQYJKoZIhvcN
AQEBBQADggIPADCCAgoCggIBAL3avv6Mv9+JHcatKLzmx3kRTqvXk/zSv0KNXynb
Q1tVpw84ei5T3pyE/geaItaZdmhYT0GwRFCNsPg8SEXQ/9WFzZx5+yBLl4ugOWE0
2rCtIzHa8cPtqZlXNLMiX6ThrvLZATGCyFBghuNvXYizBthlTwokWySE6ONvf1Ct
KsSPzZIb+U/YlBx58RinwcavLb/pG08kFXML9+Tr/Pni1dKxjhl3dbh2Eq8JiVX7
D2gIJibtcO4v5RECXxm4FLfQvPYrJbf87XUTKvuTiR8skq5Vz3WeSXGtwctNwRNU
rhSyE+c8RhkmBtMwv24NAMMhNSe5HXiAtYbpKl7cyHXzFtE1IDk4ppUCKnJHoSkQ
fgFGAUpCpf9k6F45VYwAMVmgnuFpi15Cf6Ref8nlQuI+8kngVM5Y3VwejBz5z0XV
UuBDcFHyq/KqnqEsY5f8Hsmv7P9Cv5JVqQ22tRk8q99p6u+EZZEY11a/Epf6DL2h
ePdM0gsxBnr0/vamkQf/6xKhrJPsb46xHXmK3qtOm+34eVoMGvh6OMqV0/BtZ1DG
FxtVzM0c7T6UbMk2CX877+ZC+fJN+pAUKC/TJe2LHm6YwcamMZOboFXQY8SUtohW
kL89uExqUJaIbgu97JOSGOQeHOkS1/f4vziGepulHy1R4cu9biEcJMEg70iqyn2H
LscZAgMBAAGjdjB0MB0GA1UdDgQWBBRD6yVEZ5vPAH2fOQunP2gKxuBvFTAfBgNV
HSMEGDAWgBRD6yVEZ5vPAH2fOQunP2gKxuBvFTAPBgNVHRMBAf8EBTADAQH/MCEG
A1UdEQQaMBiCCyouYm9yZy5jdWJlgglib3JnLmN1YmUwDQYJKoZIhvcNAQELBQAD
ggIBAEO/0mp2Jksi4WJwxZakrvC8YwcvqXbJtAAemuSpV9TrL2BDZbCOben1CWb4
Weh67MP5YuVrencgE/jB4wUiuOesjESXytdzm9/KQxh4SMPw5NTO8+v6Z09j3/Nc
lmBuLPutT2+kKuYOMftbeP1k1SOHBeFF+vBbPiWjjV240AaDy4/Uql+etM6fDIN4
CpJWC7XcHJMDNcEZVw8rqXHtNl096uT8BQ0+9U+sDIPncTigJlbRJ5IFQOxz0bHM
PwVfq8ySNlsjJxNbzDTo2P08rsJVXs45mBhFQl8OAO+01ennlqDBYVlDxTAh9f/L
DgxdyN4AOOGNuqwxsrLA8ngVtNa4J1qycUXAcD11yfItqrwDNrUWE1H4Ow195794
/Ci5ijyUz10tBDRLknXY/r6w4/KFidL/YsuD6p37DAcJhILm47bl4D6Z+4T0G5Q0
YUsM2cO6gkaRcDDd/OHBIykvGQTZL4hEs7ne1CKffyS8LTHF/VrWmkh35K9o6Dy1
Oxk994IuH3sKVRkLUCi/DCWHIFpXTY9tLFG1kGm6uGijLeAD54hvjuO3hSnArknV
e++szYeZXIUnYawvwoWFNnvqLdXVmbrHwauKXXd80/VZa3f3PrMH0YrGCDPIZzog
1/ip9yJCXztxjng+jHXn/FsActNh6N/3e72voqLz1X7fOUDX
-----END CERTIFICATE-----
''
  ];

  hardware.system76.enableAll = true;

  # services.jellyfin.enable = true;
  # services.jellyfin.openFirewall = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.consoleMode = "auto";
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = ["zfs"];

  boot.zfs.extraPools = [ "zfs-archive" ];

  services.zfs = {
    trim.enable = true;
    autoScrub.enable = true;
  };

  services = {
    upower.enable = true;
    avahi.enable = true;
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };
    blueman.enable = true;
    udisks2.enable = true;
  };
  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  networking.hostId = "7337c56e";

  fileSystems."/var/media/media01" = {
    device = "/dev/disk/by-label/media_01";
    fsType = "ext4";
    options = ["noatime" "nodiratime" "discard"];
  };
  fileSystems."/var/media/media02" = {
    device = "/dev/disk/by-label/media_02";
    fsType = "ext4";
    options = ["noatime" "nodiratime" "discard"];
  };
  fileSystems."/var/media/media03" = {
    device = "/dev/disk/by-label/media_03";
    fsType = "ext4";
    options = ["noatime" "nodiratime" "discard"];
  };
  fileSystems."/var/media/media04" = {
    device = "/dev/disk/by-label/media_04";
    fsType = "ext4";
    options = ["noatime" "nodiratime" "discard"];
  };

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";
  time.timeZone = "America/Chicago";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;
  networking.hostName = "fillory";
  networking.networkmanager.enable = true;

  fonts.packages = let
    nerdfonts = with pkgs.nerd-fonts; [
      ubuntu
      ubuntu-sans
      ubuntu-mono
      terminess-ttf
      symbols-only
      roboto-mono
      noto
      liberation
    ];
    stdfonts = with pkgs; [
      source-code-pro
      fira-code
      fira-code-symbols
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      liberation_ttf
      dina-font
      proggyfonts
      symbola
      aegyptus
      sigi
      material-design-icons
      hasklig
      font-awesome
    ];
  in nerdfonts ++ stdfonts;

  fonts.fontconfig = {
    enable = true;
    antialias = true;
  };


  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.videoDrivers = ["nvidia"];
  hardware.graphics.enable = true;
  # hardware.nvidia.open = false;
  hardware.nvidia = {
    open = false;
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    nvidiaSettings = true;

  };

  services.libinput.enable = true;
  # services.xserver.displayManager.gdm.enable = true;
  services.displayManager.sddm.enable = true;
  services.xserver = {
    xkb.layout = "us";
    xkb.options = "ctrl:nocaps";
    windowManager.xmonad.enable = true;
    # desktopManager.plasma5 = { enable = true; useQtScaling = true; };
    # desktopManager.gnome.enable = true;
    # desktopManager.cinnamon.enable = true;
    # desktopManager.enlightenment.enable = true;
    # desktopManager.xfce.enable = true;
    # desktopManager.mate.enable = true;
  };
  # Enable the GNOME Desktop Environment.
  # services.xserver.desktopManager.gnome.enable = true;


  hardware.bluetooth =
    { enable = true;
      package = pkgs.bluez;
      settings =
        { General =
            { Enable = "Source,Sink,Media,Socket";
            };
        };
    };


  services.fstrim.enable = true;
  fileSystems."/".options = ["noatime" "nodiratime" "discard"];

  security.rtkit = {
    enable = true;
  };

  services.pipewire = {
    enable = true;
    audio.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    extraConfig.pipewire."90-virtual-sinks" = {
      "context.objects" = [
        {
          factory = "adapter";
          args = {
            "factory.name"     = "support.null-audio-sink";
            "node.name"        = "FilterSink";
            "node.description" = "Filtered Output";
            "media.class"      = "Audio/Sink";
            "audio.position"   = "FL,FR";
          };
        }
        {
          factory = "adapter";
          args = {
            "factory.name"     = "support.null-audio-sink";
            "node.name"        = "MaxFilterSink";
            "node.description" = "Max Thump Filtered Output";
            "media.class"      = "Audio/Sink";
            "audio.position"   = "FL,FR";
          };
        }
      ];
    };
  };

  services.tailscale.enable = true;

  services.resolved = {
    enable = true;
    dnssec = "true";
    domains = ["~."];
    fallbackDns = ["192.168.50.1 # local network DNS"];
  };

  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  programs.obs-studio = {
    enable = true;
    enableVirtualCamera = true;
    plugins = with pkgs.obs-studio-plugins; [
      wlrobs
      obs-backgroundremoval
      obs-pipewire-audio-capture
      obs-source-record
    ];
  };

  users.users.rebecca = {
    isNormalUser = true;
    home = "/home/rebecca";
    description = "Rebecca Skinner";
    extraGroups = ["wheel" "networkmanager" "libvirtd"];
  };

  environment.systemPackages = with pkgs;
    [ wget
      vim
      git
      firefox
      xorg.xrandr
      bluez-tools
      pavucontrol
      system76-firmware
      virt-manager
      qemu_kvm
      qemu-utils
      qemu
      tunctl
      ethtool
      virt-top
      virt-viewer
      man-pages
      man-pages-posix
      pciutils
      usbutils
      scowl
      ripgrep
      bottom
      smartmontools
      zfstools
      zfs
      tailscale
      exfatprogs

      # cuda
      autoAddDriverRunpath
      autoFixElfFiles

      # pipewire
      pipewire
    ];

  documentation.dev.enable = true;
  programs.steam.enable = true;
  programs.ssh.startAgent = true;


  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
  services.openssh = {
    enable = true;
    openFirewall = false;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  networking.firewall.enable = true;

  networking.firewall.allowedTCPPorts = [ 41251 ];

  networking.firewall.extraCommands = ''
  iptables -A nixos-fw -p tcp --source 192.168.50/24 --dport 22:22 -j nixos-fw-accept || true
  iptables -A nixos-fw -p tcp --source 192.168.50/24 --dport 22:22 -j nixos-fw-accept || true
'';

  networking.firewall.extraStopCommands = ''
  iptables -D nixos-fw -p tcp --source 192.168.50/24 --dport 22:22 -j nixos-fw-accept || true
'';


  networking.firewall.allowedUDPPorts = [];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
