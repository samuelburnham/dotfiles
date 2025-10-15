# TODO:
# File explorer or easy navigation between directories within Nvim
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
# Change terminal cursor in insert mode to thin line, same as normal buffer
{
  pkgs,
  pkgs-unstable,
  lib,
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

        # TODO: Add https://github.com/nvim-telescope/telescope-fzf-native.nvim if better perf needed
        # https://github.com/NotAShelf/nvf/blob/9df9d51fd9fc8f9a8fc377f984ea3b7ae796172d/modules/plugins/utility/telescope/telescope.nix#L232-L243
        telescope = {
          enable = true;
          setupOpts = {
            pickers = {
              buffers = {
                sort_mru = true;
                sort_lastused = true;
              };
            };
          };
        };
        # TODO:
        # Improve icons for git, yellow circle and square is weird
        filetree.neo-tree = {
          enable = true;
          setupOpts = {
            window = {
              mappings = {
                "<space>" = "none";
              };
            };
          };
        };

        # Look into:
        # folke/trouble.nvim
        # nvim-lua/plenary.nvim
        # https://github.com/AndrewRadev/switch.vim
        # https://github.com/andymass/vim-matchup
        # https://github.com/rmagatti/goto-preview

        extraPlugins = {
          # Could use smooth scroll with mouse wheel, see https://github.com/karb94/neoscroll.nvim/issues/50#issuecomment-1160094214
          neoscroll = {
            package = pkgs.vimPlugins.neoscroll-nvim;
            setup = ''
              neoscroll = require('neoscroll')
              neoscroll.setup({
                stop_eof = true,
                cursor_scrolls_alone = false,
              })
              local keymap = {
                ["<C-S-Up>"] = function() neoscroll.ctrl_u({ duration = 250 }) end;
                ["<C-S-Down>"] = function() neoscroll.ctrl_d({ duration = 250 }) end;
                ["<PageUp>"] = function() neoscroll.ctrl_b({ duration = 450 }) end;
                ["<PageDown>"] = function() neoscroll.ctrl_f({ duration = 450 }) end;
                ["<C-Up>"] = function() neoscroll.scroll(-0.1, { move_cursor=false; duration = 100 }) end;
                ["<C-Down>"] = function() neoscroll.scroll(0.1, { move_cursor=false; duration = 100 }) end;
              }
              local modes = { 'n', 'v', 'x' }
              for key, func in pairs(keymap) do
                vim.keymap.set(modes, key, func)
              end
            '';
          };
        };
        lazy.plugins = {
          "lean.nvim" = {
            package = pkgs-unstable.vimPlugins.lean-nvim;
            setupModule = "lean";
            setupOpts = {mappings = true;};
            event = ["BufReadPre *.lean" "BufNewFile *.lean"];
          };
        };
        clipboard = {
          enable = true;
          registers = "unnamedplus";
        };
        undoFile.enable = true;
        utility.undotree = {
          enable = true;
        };
        # TODO: Save open files, possibly with session management
        #preventJunkFiles = true;

        # Lower case chars will match on upper-case as well
        searchCase = "smart";

        # TODO: Test keybindings further, probably rebind swap-buffers: https://github.com/mrjones2014/smart-splits.nvim
        # Move cursor between buffers with Ctrl+hjkl
        # Resize with Alt+hjkl
        # Swap buffers with <leader><leader>hjkl
        utility.smart-splits = {
          enable = true;
          keymaps = {
            swap_buf_left = "<leader>wh";
            swap_buf_down = "<leader>wj";
            swap_buf_up = "<leader>wk";
            swap_buf_right = "<leader>wl";
          };
        };

        # Leader key
        globals.mapleader = " ";
        keymaps = [
          {
            key = "k";
            mode = ["n" "v" "x"];
            action = "gk";
            # Executes command without displaying it on the command line
            silent = true;
            desc = "Scroll up a visual line";
          }
          {
            key = "j";
            mode = ["n" "v" "x"];
            action = "gj";
            silent = true;
            desc = "Scroll down a visual line";
          }
          {
            key = "<leader><Tab>";
            mode = ["n" "v"];
            action = ":b#<CR>";
            silent = true;
            # Description is shown by which-key on the leader popup
            desc = "Switch to most recent buffer";
          }
          {
            key = "<leader>b";
            mode = ["n" "v"];
            action = "";
            silent = true;
            desc = "Buffers";
          }
          {
            key = "<leader>bn";
            mode = ["n" "v"];
            action = ":enew<CR>";
            silent = true;
            desc = "New buffer in this window";
          }
          {
            key = "<leader>b/";
            mode = ["n" "v"];
            action = ":vnew<CR>";
            silent = true;
            desc = "New buffer split right";
          }
          {
            key = "<leader>b-";
            mode = ["n" "v"];
            action = ":new<CR>";
            silent = true;
            desc = "New buffer split below";
          }
          {
            # Deletes the buffer, force-deleting if it's a terminal
            key = "<leader>bd";
            mode = ["n" "v"];
            lua = true;
            action = "
              function()
                if vim.bo.buftype == 'terminal' then
                  vim.cmd('bd!')
                else
                  vim.cmd('bd')
                end
              end
            ";
            silent = true;
            desc = "Delete buffer";
          }
          {
            key = "<leader>bt";
            mode = ["n" "v"];
            action = "<cmd>vsp | terminal<CR>";
            silent = true;
            desc = "Open terminal to the right";
          }
          {
            key = "<Esc><Esc>";
            mode = ["t"];
            action = "<C-\\><C-n>";
            silent = true;
            desc = "Exit terminal mode";
          }
          {
            key = "<leader>w";
            mode = ["n" "v"];
            action = "";
            silent = true;
            desc = "Windows";
          }
          {
            key = "<leader>w/";
            mode = ["n" "v"];
            action = ":vsp<CR>";
            silent = true;
            desc = "New window split right";
          }
          {
            key = "<leader>w-";
            mode = ["n" "v"];
            action = ":sp<CR>";
            silent = true;
            desc = "New window split below";
          }
          {
            key = "<leader>wd";
            mode = ["n" "v"];
            action = ":close<CR>";
            silent = true;
            desc = "Close window";
          }
          {
            key = "<Esc>";
            mode = ["n"];
            action = "<cmd>nohlsearch<CR>";
            silent = true;
            desc = "Turn off search highlighting";
          }
          {
            key = "<leader>t";
            mode = ["n" "v"];
            action = ":Neotree<CR>";
            silent = true;
            desc = "Open filetree";
          }
          {
            key = "<leader>u";
            mode = ["n"];
            action = "";
            silent = true;
            desc = "Undo Tree";
          }
          {
            key = "<leader>ut";
            mode = ["n"];
            action = ":UndotreeToggle<CR>";
            silent = true;
            desc = "Toggle Undo Tree";
          }
          {
            key = "<leader>l";
            mode = ["n"];
            action = "";
            silent = true;
            desc = "LSP";
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
          lightbulb.enable = false;
          #lspSignature.enable = true;
          lspkind.enable = true;
          #mappings = {
          #
          #};
        };
        languages.nix = {
          enable = true;
          # Breaks indentation of comments and new lines
          # Disabling along with the tabstop autcmd below means tabbing on newline won't auto-indent to previous line's indent
          treesitter.enable = false;
          # Formats using alejandra v4.0.0
          format = {
            enable = true;
          };
          lsp = {
            enable = true;
          };
        };
        autocmds = [
          {
            enable = true;
            desc = "Tabs into 2 spaces for Nix";
            event = ["FileType"];
            pattern = ["nix"];
            callback = lib.generators.mkLuaInline ''
              function()
                vim.opt_local.shiftwidth =  2
                vim.opt_local.tabstop =  2
                vim.opt_local.softtabstop =  2
              end
            '';
          }
        ];
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
