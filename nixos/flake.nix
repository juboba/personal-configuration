{
  inputs = {
    homeManager = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
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
        (import ./amd-controller/amd-controller.nix)
        ./hardware-configuration.nix
        (import ./configuration.nix { inherit nixpkgs; })
      ];
    };

  };
}
