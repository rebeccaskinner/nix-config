# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ inputs }:
{ config, pkgs, ... }:

let

systemVim = (pkgs.vim_configurable.override {}).customize {
  name = "vim";
  vimrcConfig.packages.myplugins = with pkgs.vimPlugins; {
    start = [ vim-nix vim-lastplace ];
    opt = [];
  };
  vimrcConfig.customRC = ''
    set nocompatible
    set backspace=indent,eol,start
    set ts=2
    set sw=2
    set et
    syntax on
    '';
};

in {
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  nix.settings.experimental-features = ["nix-command" "flakes"];

  imports =
    [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./dns.nix
    ./nginx.nix
    ./nextcloud.nix
    ./jellyfin.nix
    # ./kiwix.nix
    ./audiobookshelf.nix
    ./miniflux.nix
    ./foundryvtt.nix
    ];

  hardware.system76.enableAll = true;

# Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.supportedFilesystems = ["zfs"];
  boot.zfs.extraPools = [ "zfs-archive" ];

  services.zfs = {
    trim.enable = true;
    autoScrub.enable = true;
  };

  services.fstrim.enable = true;
  fileSystems."/".options = ["noatime" "nodiratime" "discard"];

  fileSystems."/var/archives/jellyfin/movies" = {
    device = "/dev/disk/by-label/movies_01";
    fsType = "ext4";
    options = ["noatime" "nodiratime"];
  };

  fileSystems."/var/archives/jellyfin/shows" = {
    device = "/dev/disk/by-label/shows_01";
    fsType = "ext4";
    options = ["noatime" "nodiratime"];
  };

  fileSystems."/var/archives/data" = {
    device = "/dev/disk/by-label/file_archives";
    fsType = "ext4";
    options = ["noatime" "nodiratime"];
  };

  fileSystems."/var/archives/sync" = {
    device = "/dev/disk/by-label/sync_share";
    fsType = "ext4";
    options = ["noatime" "nodiratime"];
  };

  services = {
    upower.enable = true;
    avahi.enable = true;
  };


# Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

# Enable swap on luks
  boot.initrd.luks.devices."luks-a8007190-9007-4688-9c4d-240b81cddf25".device = "/dev/disk/by-uuid/a8007190-9007-4688-9c4d-240b81cddf25";
  boot.initrd.luks.devices."luks-a8007190-9007-4688-9c4d-240b81cddf25".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "daystrom"; # Define your hostname.
# networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

# Configure network proxy if necessary
# networking.proxy.default = "http://user:password@proxy:port/";
# networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

# Enable networking
    networking.networkmanager.enable = true;

# needed for ZFS
    networking.hostId = "d74a9a1b";

# Set your time zone.
  time.timeZone = "America/Chicago";

# Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

# Configure keymap in X11
  services.xserver = {
    xkb.layout = "us";
    xkb.variant = "";
    xkb.options = "ctrl:nocaps";
  };

# Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.rebecca = {
    isNormalUser = true;
    description = "Rebecca";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
  };

# Allow unfree packages
  nixpkgs.config.allowUnfree = true;

# List packages installed in system profile. To search, run:
# $ nix search wget
  environment.systemPackages = with pkgs; [
    openssl
      wget
      systemVim
      git
      system76-firmware
      # qemu_kvm
      # qemu-utils
      # qemu
      tunctl
      ethtool
      # virt-top
      # virt-viewer
      # man-pages
      # man-pages-posix
      pciutils
      usbutils
      scowl
      ripgrep
      bottom
      smartmontools
      tmux
      zfstools
      zfs

     tailscale
#  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
#  wget
      ];

  services.tailscale.enable = true;

  documentation.dev.enable = true;

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
    openFirewall = false;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
    };
  };
  networking.firewall.enable = true;
  networking.firewall.extraCommands = ''
  iptables -A nixos-fw -p tcp --source 192.168.50.0/24 --dport 22:22 -j nixos-fw-accept || true
'';
  networking.firewall.extraStopCommands = ''
  iptables -D nixos-fw -p tcp --source 192.168.50.0/24 --dport 22:22 -j nixos-fw-accept || true
'';
  networking.firewall.allowedUDPPorts = [ 53 ];
  networking.firewall.allowedTCPPorts = [ 53 80 443 30000 ];

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
  system.stateVersion = "22.11"; # Did you read the comment?

}
