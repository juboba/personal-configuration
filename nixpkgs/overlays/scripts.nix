self: super: {
  juboba-scripts = super.stdenv.mkDerivation { 
    name = "juboba-scripts";

    src = fetchGit {
      name = "scripts";
      url = "https://github.com/juboba/scripts.git";
      rev = "a0c893047766a261e2aae079474de539f5e17ce8";
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
