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
        extraPackages = with pkgs; [
          ripgrep
        ];
        globals.mapleader = " ";
        vimAlias = true;
        lsp.enable = true;
        extraPlugins = {
          lualine = {
            package = pkgs.vimPlugins.lualine-nvim;
            setup = "require('lualine').setup {}";
          };
        };
        clipboard = {
          enable = true;
          registers = "unnamedplus";
        };
        undoFile.enable = true;
        searchCase = "smart";
        keymaps = [
          {
            key = "<leader>w";
            mode = ["n"];
            action = ":w<CR>";
            silent = true;
            desc = "Save file";
          }
        ];
        # TODO: Modeline icons and general nerd font support (already installed in home.nix)
        #utility.icon-picker.enable = true;
      };
      #vim.globals.mapleader = " ";
    };
  };
}
