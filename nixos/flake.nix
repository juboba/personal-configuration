{
  inputs = {
    amd-controller = {
      owner = "ajmasia";
      ref = "rolling";
      repo = "amd-controller";
      type = "github";
    };

    homeManager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";

  };

  outputs = inputs @ { homeManager, nixpkgs, ... }:
  let
    system = "x86_64-linux";
  in {

    homeConfigurations.juboba = homeManager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      modules = [
        ./home.nix
      ];
    };

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
        ./hardware-configuration.nix
        (import ./configuration.nix { inherit nixpkgs; })
        {
          nixpkgs = {
            config = {
              allowUnfreePredicate = a: true;

              permittedInsecurePackages = [
                "openssl-1.1.1w"
              ];
            };

            overlays = [
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
                cypress = self.callPackage ./cypress {};
              })

              (self: super: {
                gsh = import (fetchGit {
                  url = "git@github.com:Genially/gsh";
                  ref = "refs/heads/main";
                  rev = "6375b537c5f20ec12eaad8f138c6f897fb5bd4f3";
                }) {};
              })
            ];
          };

        }
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.juboba = import ./home.nix;
          };
        }
      ];
    };
  };
}
