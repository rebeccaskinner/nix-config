# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/master.tar.gz";
in
{
  services.tailscale.enable = true;

  nixpkgs.config.allowUnfree = true;

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./mercury.nix
      (import "${home-manager}/nixos")
    ];

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

  boot.loader.systemd-boot = {
    enable = true;
    consoleMode = "auto";
  };

  boot.loader.efi.canTouchEfiVariables = true;
  networking.hostName = "brakebills";
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  networking.interfaces.enp40s0.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

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
      noto-fonts-color-emoji
      liberation_ttf
      dina-font
      proggyfonts
    ];
    fontconfig = {
      enable = true;
      antialias = true;
    };
  };

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
