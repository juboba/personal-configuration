{ ... }:

{
  programs.nixvim = {
    plugins = {
      obsidian = {
        enable = true;

        dir = "~/synologyDrive/obsidian/extended-mind";

        noteIdFunc =
          # lua
          ''
            function(title)
              -- Create note IDs in a Zettelkasten format with a timestamp and a suffix.
              local suffix = ""
              if title ~= nil then
                -- If title is given, transform it into valid file name.
                title = title
                  :gsub("á", "a")
                  :gsub("é", "e")
                  :gsub("í", "i")
                  :gsub("ó", "o")
                  :gsub("ú", "u")
                  :gsub("Á", "A")
                  :gsub("É", "E")
                  :gsub("Í", "I")
                  :gsub("Ó", "O")
                  :gsub("Ú", "U")
                suffix = title:gsub(" ", "_"):gsub("[^A-Za-z0-9_]", ""):lower()
              else
                -- If title is nil, just add 4 random uppercase letters to the suffix.
                for _ = 1, 4 do
                  suffix = suffix .. string.char(math.random(65, 90))
                end
              end
              return suffix .. "_" .. tostring(os.time())
            end,
          '';

        completion = {
          nvimCmp = true;
        };

        mappings = { };
      };
    };
  };
}
