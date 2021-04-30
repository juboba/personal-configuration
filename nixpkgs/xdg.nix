{ pkgs, ... }:

with pkgs; {
  enable = true;

  configFile = {
    "doom" = {
      recursive = true;
      source = ./dotfiles/xdg-configs/doom;
    };

    "fusuma" = {
      recursive = true;
      source = ./dotfiles/xdg-configs/fusuma;
    };

    "gsimplecal" = {
      recursive = true;
      source = ./dotfiles/xdg-configs/gsimplecal;
    };

    "nnn" = {
      recursive = true;
      source = ./dotfiles/xdg-configs/nnn;
    };

    "ranger" = {
      recursive = true;
      source = ./dotfiles/xdg-configs/ranger;
    };

    "rofi/plugins/rofiemoji" = {
      source = fetchFromGitHub {
        name = "rofiemoji";
        owner = "nkoehring";
        repo = "rofiemoji";
        rev = "ad61572830c9d3c00e30eec078d46dad3cfdb4a2";
        sha256 = "16rhb2cs8cqwflkcyw5dr77alp5wik4bv1dg66m4hkgcplxv0dx0";
      };
    };

    "sxiv" = {
      recursive = true;
      source = ./dotfiles/xdg-configs/sxiv;
    };

    "tmux/plugins/prefix-highlight" = {
      source = fetchFromGitHub {
        name = "tmux-prefix-highlight";
        owner = "tmux-plugins";
        repo = "tmux-prefix-highlight";
        rev = "15acc6172300bc2eb13c81718dc53da6ae69de4f";
        sha256 = "08rkflfnynxgv2s26b33l199h6xcqdfmlqbyqa1wkw7h85br3dgl";
      };
    };

    "tmux/plugins/extrakto" = {
      source = fetchFromGitHub {
        name = "extrakto";
        owner = "laktak";
        repo = "extrakto";
        rev = "de8ac3e8a9fa887382649784ed8cae81f5757f77";
        sha256 = "0mkp9r6mipdm7408w7ls1vfn6i3hj19nmir2bvfcp12b69zlzc47";
      };
    };

    "tmux/themes" = {
      source = (fetchFromGitHub {
        name = "tmux-themepack";
        owner = "jimeh";
        repo = "tmux-themepack";
        rev = "7c59902f64dcd7ea356e891274b21144d1ea5948";
        sha256 = "1kl93d0b28f4gn1knvbb248xw4vzb0f14hma9kba3blwn830d4bk";
      });
    };
  };
}
