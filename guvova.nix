{ pkgs, ... }:

let
  HOME_PATH = builtins.getEnv "HOME";
in
with pkgs; {
  home = {
    stateVersion = "20.09";
  };

  wayland.windowManager.hyprland = {
    enable = true;

    extraConfig = ''
      bind = SUPER, Return, exec, alacritty
    '';

    /*
      settings = {
      "$mod" = "SUPER";
      };
    */
  };
}
