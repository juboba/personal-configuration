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

  emacs = {
    enable = true;
  };

  flameshot.enable = true;

  fusuma = {
    enable = false;

    settings = {
      swipe = {
        "3" = {
          left = {
            command = "xdotool key alt+Right";
          };

          right = {
            command = "xdotool key alt+Left";
          };

          up = {
            command = "xdotool key super";
          };

          down = {
            command = "xdotool key Escape";
          };
        };

        "4"= {
          left = {
            command = "xdotool key super+Right";
          };
          right = {
            command = "xdotool key super+Left";
          };
        };
      };

      pinch = {
        "in"= {
          command= "xdotool key ctrl+plus";
        };
        out= {
          command= "xdotool key ctrl+minus";
        };
      };

      threshold= {
        swipe= 0.4;
        pinch= 0.4;
      };

      interval= {
        swipe= 0.8;
        pinch= 0.1;
      };
    };
  };

  grobi = {
    enable = false;
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
    enable = true;

    fade = true;
    fadeDelta = 5;

    settings = {
      corner-radius = 12;

      rounded-corners-exclude = [
        "class_i = 'xmobar'"
        "class_g = 'trayer'"
      ];

      shadow-exclude = [
        "class_g = 'trayer'"
      ];
    };

    shadow = true;
    shadowOpacity = 0.9;
  };

  redshift = {
    enable = true;
    #provider = "geoclue2";

    settings = {
      redshift.brightness-day = 1;
      redshift.brightness-night = 0.8;
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
      geometry  = "6x1-0+0";

      background = "#000000";
      icon_size = 22;
      kludges = "force_icons_size";
      grow_gravity = "E";
    };
  };

  trayer = {
    enable = true;

    settings = {
      align = "right";
      alpha = 50;
      distance = 80;
      distancefrom = "right";
      edge = "top";
      height = 24;
      margin = 10;
      tint = "0x00000000";
      transparent = true;
      width = 6;
      widthtype = "request";
    };
  };
}
