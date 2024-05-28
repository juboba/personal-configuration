{
  inputs = {
    amd-controller = {
      owner = "ajmasia";
      ref = "rolling";
      repo = "amd-controller";
      type = "github";
    };

    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    homeManager = {
      url = "github:nix-community/home-manager/master";
      #url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixvim = {
      url = "github:nix-community/nixvim/nixos-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix.url = "github:Mic92/sops-nix";
  };

  outputs = inputs @ { homeManager, nixpkgs, nixpkgs-unstable, ... }:
    let
      system = "x86_64-linux";
    in
    {

      nixosConfigurations.faraday = nixpkgs.lib.nixosSystem {
        inherit system;
        # Things in this set are passed to modules and accessible
        # in the top-level arguments (e.g. `{ pkgs, lib, inputs, ... }:`).
        specialArgs = {
          inherit inputs;
        };

        modules = [
          homeManager.nixosModules.home-manager
          inputs.amd-controller.module
          #inputs.nixvim.nixosModules.nixvim
          ./hardware-configuration.nix
          (import ./configuration.nix { inherit nixpkgs; inherit nixpkgs-unstable; })
          {
            nixpkgs = {
              config = {
                allowUnfreePredicate = a: true;

                permittedInsecurePackages = [
                  "openssl-1.1.1w"
                ];
              };

              overlays = [
                inputs.emacs-lsp-booster.overlays.default
                (self: super: {
                  juboba-bin = super.stdenv.mkDerivation {
                    name = "juboba-binaries";

                    src = ./bin;

                    dontPatchShebangs = true;

                    installPhase = ''
                      mkdir -p $out/bin
                      mv * $out/bin
                    '';
                  };
                })

                (self: super: {
                  cypress = self.callPackage ./cypress { };
                })

                (self: super: {
                  gsh = import
                    (fetchGit {
                      url = "git@github.com:Genially/gsh";
                      ref = "refs/heads/main";
                      rev = "07cf9e12acc85411f5579b72630784db3748f69d";
                    })
                    { };
                })
              ];
            };
          }
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.juboba = import ./home.nix;
              users.guvova = import ./guvova.nix;
              sharedModules = [
                inputs.sops-nix.homeManagerModules.sops
              ];
            };
          }
        ];
      };
    };
}
