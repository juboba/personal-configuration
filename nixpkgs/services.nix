{ ... }:
{
    blueman-applet.enable = true;

    clipmenu.enable = true;

    dunst = {
      enable = true;
      settings = import ./dunst.nix;
    };

    dropbox = {
      enable = true;
      path = "/home/juboba/Documents/Org/Dropbox";
    };

    flameshot.enable = true;

    grobi = {
      enable = true;
      rules = let LAPTOP_SCREEN = "eDP";
      HOME_SCREEN = "HDMI-A-0";
      in [
        {
          name = "Solo";
          outputs_connected = [ LAPTOP_SCREEN ];
          #outputs_disconnected = [ "DP-1" "DP-2" ];
          outputs_disconnected = [ HOME_SCREEN ];
          configure_single = LAPTOP_SCREEN + "@1920x1080";
          primary = true;
          atomic = true;

          execute_after = [
            "~/.fehbg"
          ];
        }
        {
          name = "Home";
          outputs_connected = [ HOME_SCREEN ];
          #configure_column = [ HOME_SCREEN HOME_SCREEN ++ "@1920x1080" ];
          configure_single = HOME_SCREEN;
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
      enable = true;

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
