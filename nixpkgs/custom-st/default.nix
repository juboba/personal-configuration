with import <nixpkgs> {};

(st.overrideAttrs (oldAttrs: rec {
  configFile = writeText "config.def.h" (builtins.readFile ./config.h);
  postPatch = "${oldAttrs.postPatch}\n cp ${configFile} config.def.h";
}))
