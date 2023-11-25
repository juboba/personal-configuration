{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  #boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems = {
    "/" =
      { device = "/dev/disk/by-uuid/b57e3283-f164-4056-adaa-d748797b015a";
        fsType = "ext4";
      };

    "/boot" =
      { device = "/dev/disk/by-uuid/D1E0-6D89";
        fsType = "vfat";
      };

    "/home" =
      { device = "/dev/disk/by-uuid/05e17de0-a2d8-401d-864f-39d51d73d1fe";
        fsType = "ext4";
      };

    "/media/kinesis" =
      { device = "/dev/disk/by-uuid/E8BC-3E91";
        fsType = "vfat";
        options = ["user" "rw" "noauto"] ;
      };

    "/media/sda1" =
      { device = "/dev/sda1";
        fsType = "vfat";
        options = ["user" "rw" "noauto"] ;
      };
  };

  swapDevices = [
    {
      device = "/dev/disk/by-uuid/1d84ec50-045b-4f88-a559-a6aabf999f35";
    }
  ];

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  hardware.bluetooth.enable = true;
}
