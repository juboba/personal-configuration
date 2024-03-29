{
  description = "My runnables";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.juboba-bin = with import nixpkgs { system = "x86_64-linux"; }; stdenv.mkDerivation { 
      name = "juboba-binaries";

      src = ./.;

      dontPatchShebangs = true;

      installPhase = ''
        mkdir -p $out/bin
        mv * $out/bin
      '';

    };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.juboba-bin;

  };
}
