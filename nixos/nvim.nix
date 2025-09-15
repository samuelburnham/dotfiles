{
  pkgs,
  lib,
  inputs,
  ...
}:
{
  imports = [ inputs.nvf.homeManagerModules.default ];

  programs.nvf = {
    enable = true;
    settings = {
      vim = {
        vimAlias = true;

        # Solarized dark theme
        theme = {
          enable = true;
          name = "solarized";
          style = "dark";
        };
        # Modeline
        statusline.lualine = {
          enable = true;
          theme = "solarized_dark";
        };

        extraPackages = with pkgs; [
          ripgrep
        ];

        #extraPlugins = {
        #  lualine = {
        #    package = pkgs.vimPlugins.lualine-nvim;
        #    setup = "require('lualine').setup {}";
        #  };
        #};
        clipboard = {
          enable = true;
          registers = "unnamedplus";
        };
        undoFile.enable = true;
        searchCase = "smart";

        # Leader keybindings
        globals.mapleader = " ";
        keymaps = [
          {
            key = "<leader>w";
            mode = ["n"];
            action = ":w<CR>";
            silent = true;
            desc = "Save file";
          }
        ];
        # Enable which-key for keybinding descriptions
        binds.whichKey = {
          enable = true;
        };
        # TODO: Maybe enabled already per-language
        #treesitter.enable = true;
        lsp = {
          enable = true;
          formatOnSave = true;
          #mappings = {
          #
          #};
        };
        languages.rust = {
          enable = true;
          treesitter.enable = true;
          #format.enable = true;
          #crates.enable = true;
          lsp = {
            enable = true;
          };
        };
        # TODO: Modeline icons and general nerd font support (already installed in home.nix)
        #utility.icon-picker.enable = true;
      };
      #vim.globals.mapleader = " ";
    };
  };
}
