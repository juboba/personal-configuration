{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";

    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
        ./amd-controller/amd-controller.nix
        ./hardware-configuration.nix
        (import ./configuration.nix { inherit nixpkgs; })
      ];
    };

  };
}
