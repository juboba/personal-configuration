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
        ../nixpkgs/home.nix
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
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;

            users.jabobo = {
              home = {
                file={
                  ".bashrc".text = ''
           echo Hello!
         '';
                };

                homeDirectory = "/home/jabobo";
                stateVersion = "20.09";
                username = "jabobo";
              };
            };
          };
        }
      ];
    };

  };
}
