{ pkgs, ... }:
{
  services = {
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
      package = pkgs.emacs29-gtk3;
    };

    espanso = {
      enable = true;
      matches = {
        default = {
          matches = [
            {
              trigger = ":nrs";
              replace = "sudo nixos-rebuild switch --impure --flake /home/juboba/repositories/personal-configuration";
            }
            {
              regex = ":rs(?P<service>.*)\\.";
              replace = "systemctl --user restart {{service}}";
            }
            {
              trigger = ":nsn";
              replace = "nix search nixpkgs ";
            }
            {
              trigger = ":nrn";
              replace = "nix run nixpkgs#";
            }
            {
              trigger = ":nshn";
              replace = "nix shell nixpkgs#";
            }
            {
              trigger = ":mcx";
              replace = "man configuration.nix";
            }
            {
              trigger = ":nfui";
              replace = "nix flake lock --update-input ";
            }
          ];
        };
      };
    };

    flameshot.enable = true;

    fusuma = {
      enable = false;

      extraPackages = [ pkgs.xdotool ];

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

      backend = "glx";
      vSync = true;

      fade = false;
      fadeDelta = 5;

      settings = {
        corner-radius = 12;

        focus-exclude = [
          "class_i = 'rofi'"
          "name = 'Picture-in-Picture'"
        ];

        inactive-dim = 0.5;

        opacity-rule = [
          "90:class_i = 'scratch-term'"
        ];

        rounded-corners-exclude = [
          "class_i = 'xmobar'"
          "class_g = 'eww-main'"
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

    trayer = {
      enable = true;

      settings = {
        align = "right";
        alpha = 50;
        distance = 110;
        distancefrom = "right";
        edge = "top";
        height = 26;
        margin = 10;
        tint = "0x00000000";
        transparent = true;
        width = 6;
        widthtype = "request";
      };
    };
  };
}
