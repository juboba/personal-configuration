self: super: {
  juboba-bin = super.stdenv.mkDerivation { 
    name = "juboba-binaries";

    src = ../bin;

    dontPatchShebangs = true;

    installPhase = ''
      mkdir -p $out/bin
      mv * $out/bin
    '';

  };
}
