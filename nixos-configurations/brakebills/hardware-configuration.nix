# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "uas" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/6acb7e0c-866a-49b1-965a-09df263e3827";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."nixosroot".device = "/dev/disk/by-uuid/2f746b9b-7f40-4319-a508-b14b52f9546c";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/57DE-85B9";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/21c70fe0-a23b-4f7f-81aa-1badd0912615"; }
    ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  # high-resolution display
  # hardware.video.hidpi.enable = lib.mkDefault true;
}
