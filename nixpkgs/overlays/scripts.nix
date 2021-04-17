self: super: {
  juboba-scripts = super.stdenv.mkDerivation { 
    name = "juboba-scripts";

    src = fetchGit {
      name = "scripts";
      url = "https://github.com/juboba/scripts.git";
      rev = "dba2beaf086038b832b7e1e92fd0b692ea4ff850";
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
