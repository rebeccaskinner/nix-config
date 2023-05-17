# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/master.tar.gz";
in
{
  nix.settings = {
    cores = 4; # NB: You may want to increase this on machines with more cores
    trusted-substituters = [ "https://cache.mercury.com/" "https://cache.nixos.org" ];
    substituters = [ "https://cache.mercury.com/" "https://cache.nixos.org" ];
    trusted-public-keys = [
      "cache.mercury.com:yhfFlgvqtv0cAxzflJ0aZW3mbulx4+5EOZm6k3oML+I="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
    experimental-features = ["nix-command" "flakes"];
  };

  services.tailscale.enable = true;

  nixpkgs.config.allowUnfree = true;

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      (import "${home-manager}/nixos")
    ];

  # home-manager.users.rebecca = import /home/rebecca/home-manager/oryx-work.nix { inherit config pkgs; };

  services.upower = {
    enable = true;
  };

  services.avahi = {
    enable = true;
  };

  services.dbus = {
    enable = true;
    packages = [ pkgs.dconf ];
  };

  services.gnome = {
    gnome-keyring.enable = true;
  };

  hardware.system76.enableAll = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      libvdpau-va-gl
    ];
  };


  # Use the systemd-boot EFI boot loader.
  #  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot = {
    enable = true;
    consoleMode = "auto";
  };

  boot.loader.efi.canTouchEfiVariables = true;
  networking.hostName = "brakebills";
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;
  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp40s0.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  time.timeZone = "America/Chicago";

  fonts = {
    enableDefaultFonts = true;
    fontDir.enable = true;
    fonts = with pkgs; [
      nerdfonts
      source-code-pro
      fira-code
      fira-code-symbols
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      liberation_ttf
      dina-font
      proggyfonts
    ];
    fontconfig = {
      enable = true;
      antialias = true;
    };
  };
#   fonts.fonts = with pkgs; [
#    nerdfonts
#    source-code-pro
#    fira-code
#    fira-code-symbols
#   noto-fonts
#   noto-fonts-cjk
#   noto-fonts-emoji
#   liberation_ttf
#   mplus-outline-fonts
#   dina-font
#   proggyfonts
#  ];

  # fonts.fontconfig = {
  #   enable = false;
  #   antialias = true;
  # };

  services.blueman.enable = true;
  services.logind.lidSwitchExternalPower = "ignore";

  # Enable the X11 windowing system.
  services.xserver = {
      enable = true;
      videoDrivers = ["nvidia"];

      desktopManager.plasma5.enable = false;

      displayManager.sddm = {
        enable = true;
      };

      windowManager.xmonad.enable = true;

      layout = "us";
      xkbOptions = "ctrl:nocaps";

      libinput = {
        enable = true;
        touchpad.disableWhileTyping = true;
      };
  };

  systemd.services = {
    upower.enable = true;
  };

  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.prime = {
    sync.enable = true;
    nvidiaBusId = "PCI:1:0:0";
    intelBusId = "PCI:0:2:0";
  };

  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };

  services.fstrim.enable = true;
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [ gutenprint gutenprintBin brlaser brgenml1lpr brgenml1cupswrapper ];

  # Enable sound.
  # sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraModules = [];
    package = pkgs.pulseaudioFull;
  };

  services.acpid = {
    enable = true;
  };

  programs.dconf.enable = true;

  users.users.rebecca = {
    isNormalUser = true;
    home = "/home/rebecca";
    description = "Rebecca Skinner";
    extraGroups = ["wheel" "networkmanager" "libvirtd" ];
  };
  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.jane = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  # };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    git
    firefox
    xorg.xrandr
    xorg.fontcursormisc
    bluez
    bluez-tools
    pavucontrol
    ethtool
    nvidia-vaapi-driver
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
  ];

  documentation.dev.enable = true;
  documentation.nixos.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
  services.openssh = {
    enable = true;
    ports = [ 2212 ];
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  services.openvpn.servers = {
    pritunl = {
      autoStart = false;
      updateResolvConf = true;
      config = builtins.readFile /home/rebecca/.creds/pritunl.ovpn;
    };
  };

  # UTC is required for Mercury to run
  # The other settings will speed up tests, at the cost of crash-safety
  services.postgresql = {
    package = pkgs.postgresql_13;
    enable = true;
    enableTCPIP = false;
    authentication = ''
      local all all trust
      host all all 127.0.0.1/32 trust
      host all all ::1/128 trust
    '';
    extraPlugins = [config.services.postgresql.package.pkgs.postgis];

    # for configuration in NixOS 20.09 or later
    settings = {
      timezone = "UTC";
      shared_buffers = 128;
      fsync = false;
      synchronous_commit = false;
      full_page_writes = false;
      max_locks_per_transaction = 1024;
      };
  };

  security.pki.certificateFiles = [(builtins.toFile "internal.mercury.com.ca.crt" ''
    internal.mercury.com
    -----BEGIN CERTIFICATE-----
    MIIDUDCCAjigAwIBAgIJAOPnjalJGnpNMA0GCSqGSIb3DQEBCwUAMB8xHTAbBgNV
    BAMMFGludGVybmFsLm1lcmN1cnkuY29tMB4XDTIwMDIxOTAyMDg1NVoXDTMwMDIx
    NjAyMDg1NVowHzEdMBsGA1UEAwwUaW50ZXJuYWwubWVyY3VyeS5jb20wggEiMA0G
    CSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCsSdAVzYQsZWO3FVhl/nIXwNnqrrUB
    hpkfBrCKspf+rRRrSf9/3G6i9enSAHSs8/HAQjUPNT+5367IfybgbFINZl2QLtyb
    QFOWe+ADskG8d1S5wVd7FhgefY+UACHd5mWG8SsAjUxO5Un6RWbVl5z3hILtxVHx
    UUGepepYVWukAoz77dYqkVM9ymy3XSxsg7CXrSbPEAIVNRxTMF2ADL/ZqSYA1A3w
    Pb55k62U7+rnOe8SbBdpS18z+koCthjaX/cWRvJ2Sg7K3BqURtVKq3GJRJPENGdc
    1nvKsH5UYCh5W641BLx89SHXFShH+pev5p7V5VX6TIrTDeq1WK2CJ1DDAgMBAAGj
    gY4wgYswHQYDVR0OBBYEFMxJZhpAC5Wh464DErgVtJla5pazME8GA1UdIwRIMEaA
    FMxJZhpAC5Wh464DErgVtJla5pazoSOkITAfMR0wGwYDVQQDDBRpbnRlcm5hbC5t
    ZXJjdXJ5LmNvbYIJAOPnjalJGnpNMAwGA1UdEwQFMAMBAf8wCwYDVR0PBAQDAgEG
    MA0GCSqGSIb3DQEBCwUAA4IBAQANVp9pjCnUyDABXrBtQGYf8p3Z13/ZvGJjvd0o
    ParXJ42kYkZvwjUjvOw4/vWtlLOx0uum6ldep1+kFVgLNFxiJ1ogbo8K8MWhel5j
    gmDNMX8ccFhWTccgXTpag3zv71bSbbEXRw0PnauyBoE2vTMCKg68LSsNaCmwRix9
    1UbJi9qhRxBZtjd0LqdX2o2tKRtWmiMJeLH2ZytqZY60EMNYwpOFAy7edE1+ZpZn
    IgWF3vBzHhQZta3BqAUg8F9OOjNj/aZ3eEA7XTbxGFrOn7MCrqKzNWqHfjunThBX
    NxZEHtlSSfOTziaZDi182WAMEBW6Ob2icKB/FW7gfgOpCVc3
    -----END CERTIFICATE-----

  '')];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  networking.firewall = {
    enable = true;
    allowPing = false;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
