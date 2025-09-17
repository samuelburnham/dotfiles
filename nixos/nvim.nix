# TODO:
# File explorer or easy navigation between directories within Nvim
# Easy switch between open buffers
# Cycle forward and back between recently opened buffers with Ctrl-I and Ctrl-O (this seems to also undo/redo but works for go-to def)
# Tmux and coding workflow (smart-splits.nvim and open terminal next to Nvim)
# Complete leader keybindings for common tasks
# Recover saved changes via swapfile or similar, but not in an annoying way
#   Currently `preventJunkFiles = false`, which means a backup `~` is saved to the current dir
#   Test `vim -r` without junk files to recover progress: https://neovim.io/doc/user/recover.html
# Test `vim.utility.direnv.enable` to sync Nvim shell env with direnv
# Fix indentation on new line, might be a Nix/LSP/treesitter issue but also could be an nvf option
# Add Vim emulation mode to terminal for navigation and search (not sure if possible in Ghostty)
# Obsidian.nvim or Neorg for note taking and project planning
# Seem Emacs config for more options
# Enable holding `x` to delete multiple chars
# Test adding an external plugin and doing some config in Lua
{
  pkgs,
  inputs,
  ...
}: {
  imports = [inputs.nvf.homeManagerModules.default];
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
        visuals.rainbow-delimiters.enable = true;
        # TODO: Test this out e.g. with gitsigns and consider nvim-hlslens for search
        visuals.nvim-scrollbar.enable = true;

        # TODO: Test the included packages and keybindings
        # gitsigns
        # hunk-nvim
        # vim-fugitive
        # git-conflict
        # gitlinker-nvim
        #git.enable = true;
        # Also look into NeogitOrg/neogit instead

        # TODO: Image renders but overlaps with text
        # See https://github.com/3rd/image.nvim/issues/287, likely fixed upstream
        # Or switch to https://github.com/folke/snacks.nvim/blob/main/docs/image.md
        utility.images.image-nvim = {
          enable = true;
          setupOpts.backend = "kitty";
        };

        extraPackages = with pkgs; [
          ripgrep
          kitty
        ];

        # blink-cmp autocompletion plugion
        autocomplete.blink-cmp = {
          enable = true;
          setupOpts.signature.enabled = true;
        };
        # Look into:
        # nvim-telescope/telescope.nvim (vim.telescope.enable)
        # folke/trouble.nvim
        # nvim-lua/plenary.nvim
        # https://github.com/AndrewRadev/switch.vim
        # https://github.com/andymass/vim-matchup
        # https://github.com/rmagatti/goto-preview
        extraPlugins = {
          # TODO: Lean
          # https://github.com/Julian/lean.nvim
          # Doesn't work on Nvim 0.11 yet, use VSCode for Lean dev
          #lean = {
          #  package = pkgs.vimPlugins.lean-nvim;
          #  event = { 'BufReadPre *.lean', 'BufNewFile *.lean' };
          #  opts = {
          #    mappings = true;
          #  };
          #};
          #  lualine = {
          #    package = pkgs.vimPlugins.lualine-nvim;
          #    setup = "require('lualine').setup {}";
          #  };
        };
        clipboard = {
          enable = true;
          registers = "unnamedplus";
        };
        undoFile.enable = true;
        utility.undotree = {
          enable = true;
        };
        #preventJunkFiles = true;
        searchCase = "smart";

        # TODO: Test keybindings further, probably rebind swap-buffers: https://github.com/mrjones2014/smart-splits.nvim
        # Move cursor between buffers with Ctrl+hjkl
        # Resize with Alt+hjkl
        # Swap buffers with <leader><leader>hjkl
        utility.smart-splits = {
          enable = true;
        };

        # Leader key
        globals.mapleader = " ";
        keymaps = [
          {
            # Window management
            key = "<leader>w";
            mode = ["n"];
            action = ":w<CR>";
            silent = true;
            desc = "Save file";
          }
          {
            # Undo tree
            key = "<leader>ut";
            mode = ["n"];
            action = ":UndotreeToggle<CR>";
            silent = true;
            desc = "Toggle Undo Tree";
          }
          # Use PgUp/PgDown or <C-f>/<C-b> for large jumps
          # <C-d>/<C-u> also jumps
          {
            key = "<C-j>";
            mode = ["n" "v"];
            action = "<C-e>";
            silent = true;
            desc = "Scroll up";
          }
          {
            key = "<C-k>";
            mode = ["n" "v"];
            action = "<C-y>";
            silent = true;
            desc = "Scroll down";
          }
        ];
        # TODO: Set a description for intermediate keybindings, e.g. `<leader>l` is LSP-related commands
        # Enable which-key for keybinding descriptions
        binds.whichKey = {
          enable = true;
        };
        lsp = {
          enable = true;
          formatOnSave = true;
          inlayHints.enable = true;
          lightbulb.enable = true;
          #lspSignature.enable = true;
          lspkind.enable = true;
          #mappings = {
          #
          #};
        };
        languages.nix = {
          enable = true;
          treesitter.enable = true;
          #format = {
          #  enable = true;
          # };
          lsp = {
            enable = true;
          };
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
    };
  };
}
