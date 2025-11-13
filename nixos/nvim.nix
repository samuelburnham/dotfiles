# TODO:
# Set up projects for each Neovim instance like VSCode or tmux, where each neovim is its own project with saved state
# Test lean.nvim with goto-def, infoview, and hover, `,` local leader key maybe should be unified with `<leader>l`
# Test `blink-cmp`
# Add some functionality borrowed from tmux like session persistence, SSH, and running a Neovim systemd server on startup. See https://kraust.github.io/posts/neovim-is-a-multiplexer/
# Complete leader keybindings for common tasks (see Emacs config)
# Obsidian.nvim or Neorg for note taking and project planning, with https://github.com/MeanderingProgrammer/render-markdown.nvim
# Seem Emacs config for more options
# Consider Neovide for GUI experience and to fully decouple from terminal
# Keep an eye on Ghostty integration with Neovim, such as https://github.com/neovim/neovim/issues/33155 which would fix multiline copy-paste from `:terminal`
# and probably other bugs like squashed text on window resize
# awesome-nvf configurations:
# https://github.com/jack-thesparrow/schrovimger
# https://github.com/e-v-o-l-v-e/nix-config/blob/main/home/nvf.nix
#
# Look into:
# folke/trouble.nvim
# nvim-lua/plenary.nvim
# https://github.com/smoka7/multicursors.nvim
# https://github.com/NeogitOrg/neogit
#
# Cheat sheet
# Terminal copy-paste uses system clipboard, `y`/`p` should work
# The latter works in insert mode as well in both regular and terminal buffer
#
# <C-Right>/<C-Left> to cycle open tabs, so that you don't open already closed buffers
# To reach several tabs away, do <leader>bc to pick tab by letter, or <leader>fb to open buffers list
#
# To go forward/backward in cursor history (where was I last), use <C-i>/<C-o>
# This will jump between buffers as well. It's nice for going back after go to def or jumping around a file
#
# Visual mode
# Follow which-key for key sequences, it's amazing
# gU/gu to uppercase or lowercase selection, g~ to toggle case
# gc to toggle comment, gcc for the current line
# gv to select last visual selection
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
        options.guicursor = "n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20,t:ver25-blinkon500-blinkoff500-TermCursor";
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

        # TODO: Don't make enter select_and_accept completion ever, just use Tab or C-Space to select
        # Enter is for new line (in file) or <CR> in cmdline
        # TODO: Unify cmdline and file-based keybindings
        # blink-cmp autocompletion plugion
        autocomplete.blink-cmp = {
          enable = true;
          setupOpts = {
            signature.enabled = true;
            cmdline.enabled = true;

            # Select and accept with `<C-space>`, close with `<C-e>`
            cmdline.keymap = {
              preset = "default";
              # TODO: Tab select_and_accept doesn't work, not sure why
              "<Tab>" = ["show" "accept"];
              "<C-space>" = ["select_and_accept" "fallback"];
            };
            cmdline.completion.list.selection.auto_insert = false;
            cmdline.completion.list.selection.preselect = true;
            # If `menu.auto_show` is annoying, can set to false and set Tab to show menu and also accept
            # `"<Tab>" = ["show_and_insert_or_accept_single" "select_and_accept"];`
            # But have to wait for blink 1.7, above doesn't work atm
            cmdline.completion.menu.auto_show = lib.generators.mkLuaInline ''
              function(ctx)
                return vim.fn.getcmdtype() == ':'
              end
            '';
          };
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
        # Fix auto-refresh after running git commands from `:terminal`
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
        ui.noice = {
          enable = true;
          setupOpts = {
            presets = {
              command_palette = false;
            };
            # Workaround enabling `:!` shell command output: https://github.com/folke/noice.nvim/issues/1097
            routes = [
              {
                filter = {
                  event = "msg_show";
                  kind = ["shell_out" "shell_err" "shell_ret"];
                };
                view = "popup";
                opts = {
                  level = "info";
                  skip = false;
                  replace = false;
                };
              }
            ];
          };
        };

        notify.nvim-notify = {
          enable = true;
        };

        # Use `direnv.nvim` instead for now to fix loading files from other directorie
        utility.direnv.enable = false;

        extraPlugins = {
          # Enables loading rust-analyzer after direnv completes for Rust files in another directory
          # Otherwise rust-analyzer can't find the binary because direnv hasn't yet loaded it from the Nix flake
          # Lean doesn't have this issue, likely due to starting via autocmd rather than `vim.lsp.enable()`
          direnv = {
            package = pkgs.vimUtils.buildVimPlugin {
              pname = "direnv.nvim";
              version = "main";
              src = pkgs.fetchFromGitHub {
                owner = "actionshrimp";
                repo = "direnv.nvim";
                rev = "0d2edd378dbdf2c653869772d761ad914219ba9d";
                hash = "sha256-p2im4nUV0n9HQsjCA9oGJvTADfKGlCEr/RYWGlUszuU=";
              };
            };
            setup = ''
              require('direnv-nvim').setup({
                on_direnv_finished = function ()
                  bufnr = vim.api.nvim_get_current_buf()
                  if vim.bo[bufnr].filetype == "rust" then
                    vim.lsp.start({
                      name = 'rust-analyzer',
                      cmd = {'rust-analyzer'},
                      root_dir = vim.fs.root(0, {'Cargo.toml'}),
                    })
                  end
                end
              })
            '';
          };
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
          # Vim launched from `:terminal` opens a buffer instead of vimception
          flatten = {
            package = pkgs-unstable.vimPlugins.flatten-nvim;
            setup = ''
              require('flatten').setup({
                window = {
                  open = "current",
                  focus = "last",
                },
              })
            '';
          };
          # lean.nvim dep, enables line comments with `gc`
          tcomment = {
            package = pkgs.vimPlugins.tcomment_vim;
          };
          # lean.nvim dep, enables enhanced `%` motion
          matchup = {
            package = pkgs.vimPlugins.vim-matchup;
          };
          # lean.nvim dep, enables switching text variants with `gs`
          switch = {
            package = pkgs.vimPlugins.switch-vim;
          };
        };
        # TODO: Look into optional lean.nvim enhancements
        # https://github.com/lewis6991/satellite.nvim
        # https://github.com/kosayoda/nvim-lightbulb
        # https://github.com/rmagatti/goto-preview
        #
        lazy.plugins = {
          "lean.nvim" = {
            package = pkgs-unstable.vimPlugins.lean-nvim;
            setupModule = "lean";
            setupOpts = {mappings = true;};
            event = ["BufReadPre *.lean" "BufNewFile *.lean"];
          };
          # TODO: Add dashboard-nvim to quickly open recent sessions on launch
          # Session manager by folke that (mostly) works with Noice
          "persistence.nvim" = {
            package = pkgs-unstable.vimPlugins.persistence-nvim;
            setupModule = "persistence";
            setupOpts = {
              dir = lib.generators.mkLuaInline "vim.fn.stdpath('data') .. '/sessions/' ";
            };
            event = ["BufReadPre"];
          };
        };
        clipboard = {
          enable = true;
          registers = "unnamedplus";
        };
        undoFile.enable = true;
        # TODO: Replace with https://github.com/gbprod/yanky.nvim
        utility.undotree = {
          enable = true;
        };

        # Preserve swapfiles in case Neovim crashes
        preventJunkFiles = false;
        # `stdpath` is at `~/.local/share/nvf/`
        options.directory = lib.generators.mkLuaInline "vim.fn.stdpath('data') .. '/swap'";
        options.backupdir = lib.generators.mkLuaInline "vim.fn.stdpath('data') .. '/backup'";

        # Using persistence.nvim for now
        session.nvim-session-manager = {
          enable = false;
        };

        # Lower case chars will match on upper-case as well
        searchCase = "smart";

        # Move cursor between buffers with Ctrl+hjkl
        # Resize with Alt+hjkl
        # Swap buffers with <leader>ws+hjkl
        utility.smart-splits = {
          enable = true;
          keymaps = {
            move_cursor_left = "<C-h>";
            move_cursor_down = "<C-j>";
            move_cursor_up = "<C-k>";
            move_cursor_right = "<C-l>";
            swap_buf_left = "<leader>wsh";
            swap_buf_down = "<leader>wsj";
            swap_buf_up = "<leader>wsk";
            swap_buf_right = "<leader>wsl";
          };
        };

        # Tab line gives a quick overview of open tabs and whether they are saved
        # Also helps with navigating open buffers, so I don't have to cycle by memory
        # or open the buffers list, which is a power tool and usually overkill
        tabline.nvimBufferline = {
          enable = true;
          setupOpts.options = {
            sort_by = "insert_at_end";
            hover = {
              enabled = true;
              delay = 100;
            };
          };
          mappings = {
            cycleNext = "<C-Right>";
            cyclePrevious = "<C-Left>";
            moveNext = "<A-Right>";
            movePrevious = "<A-Left>";
          };
        };
        # Needed for bufferline's hover on tab event showing the close icon
        options.mousemoveevent = true;

        mini.bufremove = {
          enable = true;
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
            # Deletes the buffer, prompting to save if changed
            # Based on https://github.com/folke/snacks.nvim/blob/main/lua/snacks/bufdelete.lua
            key = "<leader>bd";
            mode = ["n" "v"];
            lua = true;
            action = ''
              function()
                local bd = require("mini.bufremove").delete
                if vim.bo.modified then
                  local choice = vim.fn.confirm(("Save changes to %q?"):format(vim.fn.bufname()), "&Yes\n&No\n&Cancel")
                  if choice == 1 then -- Yes
                    local ok, out = pcall(vim.api.nvim_command_output, "write")
                    bd(0)
                    if ok and out ~= "" then
                      vim.schedule(function()
                        vim.api.nvim_echo({{out, "Normal"}}, false, {})
                      end)
                    end
                  elseif choice == 2 then -- No
                    bd(0, true)
                  end
                else
                  bd(0)
                end
              end
            '';
            silent = false;
            desc = "Delete buffer";
          }
          {
            key = "<leader>bD";
            mode = ["n" "v"];
            lua = true;
            action = ''
              function()
                local bd = require("mini.bufremove").delete(0, true)
              end
            '';
            silent = false;
            desc = "Delete buffer (force)";
          }
          {
            key = "<leader>bt";
            mode = ["n" "v"];
            action = "<cmd>vsp | terminal<CR>";
            silent = true;
            desc = "Open terminal to the right";
          }
          {
            key = "<Esc>";
            mode = ["t"];
            action = "<C-\\><C-n>";
            silent = true;
            desc = "Exit terminal mode";
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
            key = "<leader>ut";
            mode = ["n"];
            action = ":UndotreeToggle<CR>";
            silent = true;
            desc = "Toggle Undo Tree";
          }
          {
            key = "<leader>sl";
            mode = ["n"];
            lua = true;
            action = ''
              function()
                require("persistence").load({ last = true })
              end
            '';
            silent = true;
            desc = "Load last session";
          }
          {
            key = "<leader>ss";
            mode = ["n"];
            lua = true;
            action = ''
              function()
                require("persistence").select()
              end
            '';
            silent = true;
            desc = "Select session to load";
          }
          {
            key = "<leader>sl";
            mode = ["n"];
            lua = true;
            action = ''
              function()
                require("persistence").load()
              end
            '';
            silent = true;
            desc = "Load session for the current directory";
          }
          {
            key = "<leader>sL";
            mode = ["n"];
            lua = true;
            action = ''
              function()
                require("persistence").load({ last = true })
              end
            '';
            silent = true;
            desc = "Load most recent session (global)";
          }
          {
            key = "<leader>sd";
            mode = ["n"];
            lua = true;
            action = ''
              function()
                require("persistence").stop()
              end
            '';
            silent = true;
            desc = "Disable persistence - don't save on exit";
          }
          {
            key = "<leader>se";
            mode = ["n"];
            lua = true;
            action = ''
              function()
                require("persistence").start()
              end
            '';
            silent = true;
            desc = "Enable persistence - will save on exit";
          }
        ];

        binds.whichKey = {
          enable = true;
          register = {
            "<leader>b" = "+Buffers";
            "<leader>u" = "+Undo Tree";
            "<leader>l" = "+LSP";
            "<leader>w" = "+Windows";
            "<leader>ws" = "+Swap windows";
          };
        };

        binds.hardtime-nvim = {
          enable = false;
          #enable = true;
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
          {
            enable = true;
            desc = "Enter insert mode when starting terminal";
            event = ["TermOpen"];
            callback = lib.generators.mkLuaInline ''
              function()
                vim.cmd("startinsert")
              end
            '';
          }
          {
            enable = true;
            desc = "Enter insert mode and restore guicursor in terminal after Git buffer";
            event = ["BufLeave"];
            pattern = ["*COMMIT_EDITMSG" "*git-rebase-todo"];
            callback = lib.generators.mkLuaInline ''
              function()
                vim.schedule(function()
                  if vim.bo.buftype == 'terminal' then
                    vim.cmd("set guicursor& | set guicursor=" .. vim.o.guicursor)
                    vim.cmd("startinsert")
                  end
                end)
              end
            '';
          }
          # Workaround due to Noice mangling swapfile messages in the UI on session restore
          # When opening a file otherwise, the swap message takes priority and prints correctly
          {
            enable = true;
            desc = "Disable Noice before loading session";
            event = ["User"];
            pattern = ["PersistenceLoadPre"];
            callback = lib.generators.mkLuaInline ''
              function()
                vim.cmd("Noice disable")
              end
            '';
          }
          {
            enable = true;
            desc = "Enable Noice after loading session";
            event = ["User"];
            pattern = ["PersistenceLoadPost"];
            callback = lib.generators.mkLuaInline ''
              function()
                vim.cmd("Noice enable")
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
            # Enabling makes the LSP start automatically via the global `vim.lsp.enable('rust-analyzer')` setting
            # This breaks when opening files from another directory because direnv hasn't finished loading rust-analyzer yet.
            # The solution is the direnv.nvim autocmd in `extraPlugins`
            enable = false;
          };
        };
        # TODO: Modeline icons and general nerd font support (already installed in home.nix and supported by Ghostty)
        #utility.icon-picker.enable = true;

        luaConfigRC.config-dir = ''
          require("config")
        '';
      };
    };
  };
}
