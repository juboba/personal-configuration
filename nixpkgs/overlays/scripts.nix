self: super: {
  juboba-scripts = super.stdenv.mkDerivation { 
    name = "juboba-scripts";

    src = fetchGit {
      name = "scripts";
      url = "https://github.com/juboba/scripts.git";
      rev = "03f3dc65e2d3304fcf7a330ee527fa1e35b751d8";
    };

    dontPatchShebangs = true;

    nativeBuildInputs = [
      self.pkgs.gawk
    ];

    installPhase = ''
      mkdir -p $out/bin
      rm README.org
      mv * $out/bin
    '';

  };
}
