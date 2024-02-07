{ ... }:
let
  statuscolum = builtins.readFile ./_lua/statuscolum.lua;
in
{
  imports = [
    ./options.nix
    ./keymaps.nix
    ./filetype.nix
  ];

  programs.nixvim = {
    extraConfigLuaPre = ''
      -- define the style of the status colum on the left screen
      ${statuscolum}
    '';

    extraFiles."queries/markdown/injections.scm" = ''
      ;; extends

      ((inline) @injection.content
        (#lua-match? @injection.content "^%s*import")
        (#set! injection.language "typescript"))
      ((inline) @injection.content
        (#lua-match? @injection.content "^%s*export")
        (#set! injection.language "typescript"))
    '';
  };
}
