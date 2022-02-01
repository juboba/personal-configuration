self: super: {
  juboba-scripts = super.stdenv.mkDerivation { 
    name = "juboba-scripts";

    src = fetchGit {
      name = "scripts";
      url = "https://github.com/juboba/scripts.git";
      rev = "fc1e450f13b17dc788f7e524b160985283fa6518";
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
