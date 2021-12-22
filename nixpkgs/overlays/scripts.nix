self: super: {
  juboba-scripts = super.stdenv.mkDerivation { 
    name = "juboba-scripts";

    src = fetchGit {
      name = "scripts";
      url = "https://github.com/juboba/scripts.git";
      rev = "b0c9f64a745ee5b2b48ed197f8a542a69e2b880f";
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
