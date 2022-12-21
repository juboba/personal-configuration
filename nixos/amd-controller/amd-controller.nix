{ config, lib, pkgs, ... }:

let cfg = config.amd-controller;
    amdController = (pkgs.callPackage ./default.nix {});
    awake = pkgs.writeShellScriptBin "awake" ''
    if [ ! -f "/sys/class/power_supply/AC0/online" ]; then
      exit 1
    fi

    sleep 10 # needed for override the BIOS default setup
    ${amdController}/bin/amd-controller set -s
  '';

    awake-udev = pkgs.writeShellScriptBin "awake-udev" ''
    if [ ! -f "/sys/class/power_supply/AC0/online" ]; then
      exit 1
    fi

    sleep 1m # needed for override the BIOS default setup
    ${amdController}/bin/amd-controller set -s
  '';

  processors = {
    "4800H" = import ./processors/4800H.nix;
  };

in {
  options.amd-controller = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = mDoc ''
        Enable AMD Controller thingy
      '';
    };

    udev.enable = mkOption {
      type = types.bool;
      default = false;
      description = mDoc ''
        Enable management of udev rules for waking from suspension...
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ amdController ];

      etc."amd-controller/config.json".source = with builtins; toFile "config" (toJSON processors."4800H");
    };

    powerManagement = {
      enable = true;

      cpuFreqGovernor = "ondemand";
      powerUpCommands = "${awake}/bin/awake";
      resumeCommands = "${awake}/bin/awake";

      powertop.enable = true;
    };

    services.udev.extraRules = lib.mkIf cfg.udev.enable ''
      # This config optimize the battery power
      SUBSYSTEM=="power_supply", KERNEL=="AC0", DRIVER=="", ATTR{online}=="1", RUN+="${awake-udev}/bin/awake-udev"
      SUBSYSTEM=="power_supply", KERNEL=="AC0", DRIVER=="", ATTR{online}=="0", RUN+="${awake-udev}/bin/awake-udev"
    '';
  };
}
