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

				{
					nixpkgs.config.allowUnfreePredicate = a: true; 
				}

				{
					home = {
						username = "juboba";
						homeDirectory = "/home/juboba";
						stateVersion = "20.09";
					};
				}
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

        ({ pkgs, ... }: {
          nix = {
            extraOptions = "experimental-features = nix-command flakes";
            package = pkgs.nixFlakes;
            registry.nixpkgs.flake = nixpkgs;
          };
          
          home-manager.useGlobalPkgs = true;
        })

        ./configuration.nix
      ];
    };

  };
}
