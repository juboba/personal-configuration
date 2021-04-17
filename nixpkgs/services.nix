{ ... }:

{
    blueman-applet.enable = true;

    clipmenu.enable = true;

    dunst = {
      enable = false;
      settings = import ./dunst.nix;
    };

    flameshot.enable = true;

    grobi = {
      enable = true;
      rules = [
        {
          name = "Solo";
          outputs_connected = [ "eDP-1" ];
          #outputs_disconnected = [ "DP-1" "DP-2" ];
          outputs_disconnected = [ "DP-1" ];
          configure_single = "eDP-1@1920x1080";
          primary = true;
          atomic = true;

          execute_after = [
            "~/.fehbg"
          ];
        }
        {
          name = "Home";
          outputs_connected = [ "DP-1" ];
          #configure_column = [ "DP-1" "eDP-1@1920x1080" ];
          configure_single = "DP-1";
          primary = true;
          atomic = true;

          execute_after = [
            "~/.fehbg"
          ];
        }
      ];
    };

    network-manager-applet.enable = true;

    picom = {
      #blur = true;
      enable = true;
      #inactiveDim = "0.5";
      #inactiveOpacity = "0.9";
    };

    redshift = {
      enable = true;
      #provider = "geoclue2";

      brightness = {
        day = "1";
        night = "0.8";
      };

      latitude = "36";
      longitude = "-6";

      temperature = {
        day = 7700;
        night = 3700;
      };

      tray = true;
    };

    stalonetray = {
      enable = false;

      config = {
        transparent = false;
        geometry  = "1x1+1050+3";

        background = "#111b1e";
        icon_size = 16;
        kludges = "force_icons_size";
        grow_gravity = "W";
      };
    };
}
