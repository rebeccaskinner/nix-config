# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/master.tar.gz";
in
{
  nixpkgs.config.allowUnfree = true;
  nix.settings.experimental-features = ["nix-command" "flakes"];

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      (import "${home-manager}/nixos")
    ];

  security.pki.certificateFiles = ["/var/certs/borg.cube.crt"];

  home-manager.users.rebecca = import /home/rebecca/home-manager/fillory.nix { inherit config pkgs; };

  hardware.system76.enableAll = true;

  # services.jellyfin.enable = true;
  # services.jellyfin.openFirewall = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.consoleMode = "auto";
  boot.loader.efi.canTouchEfiVariables = true;

  boot.zfs = {
    enabled = true;
  };

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

  fonts.packages = with pkgs; [
    nerdfonts
    source-code-pro
    fira-code fira-code-symbols noto-fonts noto-fonts-cjk noto-fonts-emoji liberation_ttf dina-font proggyfonts symbola aegyptus
  ];

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
  hardware.opengl.enable = true;


  services.xserver = {
    displayManager.sddm.enable = true;
    layout = "us";
    xkbOptions = "ctrl:nocaps";
    libinput.enable = true;
    windowManager.xmonad.enable = true;
    # desktopManager.plasma5 = { enable = true; useQtScaling = true; };
    # desktopManager.gnome.enable = true;
    # desktopManager.cinnamon.enable = true;
    # desktopManager.enlightenment.enable = true;
    # desktopManager.xfce.enable = true;
    # desktopManager.mate.enable = true;
  };
  # Enable the GNOME Desktop Environment.
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome.enable = true;


  hardware.bluetooth = { enable = true; settings = { General = { Enable = "Source,Sink,Media,Socket"; }; }; };


  services.fstrim.enable = true;
  fileSystems."/".options = ["noatime" "nodiratime" "discard"];

  hardware.pulseaudio = { enable = true; package = pkgs.pulseaudioFull; };
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

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
      bluez
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
    ];

  documentation.dev.enable = true;
  programs.steam.enable = true;


  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.jane = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  # };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #   wget
  #   firefox
  # ];

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
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  networking.firewall.enable = true;

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
