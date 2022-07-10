{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";

    juboba-bin.url = "path:../nixpkgs/bin";

    homeManager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { homeManager, juboba-bin, nixpkgs, ... }: 
  let
      system = "x86_64-linux";
  in {

    homeConfigurations.juboba = homeManager.lib.homeManagerConfiguration {
      inherit system;
      stateVersion = "20.09";
      username = "juboba";
      homeDirectory = "/home/juboba";
      configuration = {...}: {
        imports = [ ../nixpkgs/home.nix ];

        nixpkgs.config.allowUnfreePredicate = a: true; 

      };
      extraSpecialArgs = {
        inherit juboba-bin;
      };
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

        ({ pkgs, ... }: {
          nix.extraOptions = "experimental-features = nix-command flakes";
          nix.package = pkgs.nixFlakes;
          nix.registry.nixpkgs.flake = nixpkgs;
          
          home-manager.useGlobalPkgs = true;
        })

        ./configuration.nix
      ];
    };

  };
}
