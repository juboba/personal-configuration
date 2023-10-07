{ pkgs }:
pkgs.stdenv.mkDerivation { 
  name = "juboba-binaries";

  src = ./.;

  dontPatchShebangs = true;

  installPhase = ''
        mkdir -p $out/bin
        mv * $out/bin
  '';
}
