# TODO:
# Make PgUp/Down map to C-u/C-d as the latter is a more useful scroll amount
# Set up projects for each Neovim instance like VSCode or tmux, where each neovim is its own project with saved state
# Test lean.nvim with goto-def, infoview, and hover, `,` local leader key maybe should be unified with `<leader>l`
# Test `blink-cmp`
# Add some functionality borrowed from tmux like session persistence, SSH, and running a Neovim systemd server on startup. See https://kraust.github.io/posts/neovim-is-a-multiplexer/
# Complete leader keybindings for common tasks (see Emacs config)
# Obsidian.nvim or Neorg for note taking and project planning, with https://github.com/MeanderingProgrammer/render-markdown.nvim
# See Emacs config for more options
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
# Follow which-key for key sequences, it's amazing. But increase menu popup time so it only shows after a second or two
# gU/gu to uppercase or lowercase selection, g~ to toggle case
# gc to toggle comment, gcc for the current line
# gv to select last visual selection
# Core Neovim configuration that can be used both standalone and with home-manager
{
  pkgs,
  pkgs-unstable,
  lib,
  inputs,
  ...
}:
{
  vim = {
    options.guicursor = "n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20,t:ver25-blinkon500-blinkoff500-TermCursor";
    vimAlias = true;

    # Solarized theme, follows OS light/dark preference via auto-dark-mode.nvim
    theme = {
      enable = true;
      name = "solarized";
      style = "solarized";
      # Diff* overrides: give added/removed lines a visible bg instead of
      # solarized.nvim's fg-only tint, and force Neogit's adds to green in
      # dark mode (solarized.nvim ships them as blue). Registered in
      # extraConfig so it's in place before `:colorscheme solarized` fires.
      extraConfig = ''
        vim.api.nvim_create_autocmd("ColorScheme", {
          pattern = "solarized",
          callback = function()
            local palette = (vim.o.background == "light")
              and require("solarized.palette.solarized-light")
              or require("solarized.palette")
            local c = palette[require("solarized").config.palette or "solarized"]
            -- solarized's light mix_red/mix_green are nearly cream-on-cream;
            -- substitute saturated pink/leaf-green there.
            local is_light = vim.o.background == "light"
            local red_bg   = is_light and "#f5c4c4" or c.mix_red
            local green_bg = is_light and "#bfd9bc" or c.mix_green
            -- DiffChange marks lines that have intra-line edits; DiffText
            -- marks the exact changed span inside them. With the empty
            -- DiffChange + bold-only DiffText we had before, single-word
            -- edits showed no color at all (bold alone on black reads as
            -- plain text). Give DiffChange a muted neutral bg and DiffText
            -- a stronger tint so the changed chars pop against the line.
            -- Orange/yellow for "changed" is the diff-UI convention and
            -- avoids confusion with DiffAdd (green) / DiffDelete (red).
            -- diffview.nvim applies the same DiffText to both sides of the
            -- split, so a side-neutral accent is the only correct choice.
            --
            -- Dark-mode fg override: without an explicit fg, DiffText
            -- inherits the underlying token's color — faded Comment gray
            -- (base01 #586e75) on our warm-brown bg is barely legible.
            -- Forcing fg = base3 (cream) ensures the changed chars pop
            -- regardless of syntax class. Syntax colors are suppressed on
            -- the changed span (one-line-diff tradeoff), but that's the
            -- right call here: the point of DiffText is to scream "this
            -- is what changed", not to preserve token coloring. Light
            -- mode keeps the default fg — pale-yellow bg has enough
            -- contrast with any solarized-light token color.
            local change_bg = is_light and "#f5ebcc" or "#3C342C"
            local text_bg   = is_light and "#e8cf79" or "#6b4a1f"
            local text_fg   = is_light and nil or c.base3
            local set = vim.api.nvim_set_hl
            set(0, "DiffAdd",    { bg = green_bg })
            set(0, "DiffDelete", { bg = red_bg, fg = c.red })
            set(0, "DiffChange", { bg = change_bg })
            set(0, "DiffText",   { bg = text_bg, fg = text_fg, bold = true })
            pcall(function() require("diffview.hl").setup() end)
            set(0, "NeogitDiffAdd",             { fg = c.green })
            set(0, "NeogitDiffAddHighlight",    { fg = c.green, bg = green_bg })
            set(0, "NeogitDiffAddInline",       { bg = green_bg, bold = true })
            set(0, "NeogitDiffDelete",          { fg = c.red })
            set(0, "NeogitDiffDeleteHighlight", { fg = c.red, bg = red_bg })
            set(0, "NeogitDiffDeleteInline",    { bg = red_bg, bold = true })
            -- solarized.nvim's dark palette maps git_add to blue (#268BD2),
            -- which collides with the usual green=add / red=delete /
            -- yellow=change convention and with our diffview palette above.
            -- Canonical solarized green (#859900) is olive and barely
            -- distinguishable from yellow (#B58900) as a sign-column fg in
            -- either mode. Substitute selenized's greens — a darker
            -- #489100 in light mode and a brighter #75b938 in dark mode —
            -- so add vs change reads at a glance.
            local sign_add = is_light and "#489100" or "#75b938"
            set(0, "GitSignsAdd",    { fg = sign_add })
            set(0, "GitSignsChange", { fg = c.yellow })
            set(0, "GitSignsDelete", { fg = c.red })
          end,
        })
      '';
    };
    # Modeline
    statusline.lualine = {
      enable = true;
      theme = "auto";
      # solarized.nvim's lualine theme doesn't define `terminal`, so it falls
      # back to normal (blue). Wrap the resolved theme and set terminal to cyan.
      setupOpts.options.theme = lib.mkForce (
        lib.generators.mkLuaInline ''
          (function()
            local name = vim.g.colors_name or 'auto'
            local ok, theme = pcall(require, 'lualine.themes.' .. name)
            if not ok then theme = require('lualine.themes.auto') end
            theme.terminal = { a = { fg = '#002b36', bg = '#2aa198', gui = 'bold' } }
            return theme
          end)()
        ''
      );
    };
    visuals.rainbow-delimiters.enable = true;
    # TODO: Test this out e.g. with gitsigns and consider nvim-hlslens for search
    visuals.nvim-scrollbar.enable = true;

    utility.diffview-nvim = {
      enable = true;
      # Asymmetric red/green per side (matches the README preview): DiffAdd
      # on the old buffer is win-remapped to a copy of DiffDelete.
      setupOpts.enhanced_diff_hl = true;
    };

    # Passive visualization only — hunk staging, reset, and diffs go
    # through Neogit/diffview. Flattened into the <leader>g namespace
    # alongside the Neogit popups (gs/gw/gc/gl/gp).
    git.gitsigns = {
      enable = true;
      mappings = {
        stageHunk = null;
        undoStageHunk = null;
        resetHunk = null;
        stageBuffer = null;
        resetBuffer = null;
        diffThis = null;
        diffProject = null;
        toggleDeleted = null;

        # ]c/[c defaults retained — they fall through to vim's native
        # next-change inside diff mode, and jump gitsigns hunks elsewhere.
        previewHunk = "<leader>gh";
        blameLine = "<leader>gb";
        toggleBlame = "<leader>gB";
      };
    };

    # TODO: Image renders but overlaps with text
    # See https://github.com/3rd/image.nvim/issues/287, likely fixed upstream
    # Or switch to https://github.com/folke/snacks.nvim/blob/main/docs/image.md
    #utility.images.image-nvim = {
    #  enable = true;
    #  setupOpts.backend = "kitty";
    #};

    utility.snacks-nvim = {
      enable = true;
      setupOpts = {
        dashboard = {
          enabled = true;
          #{ section = "projects", gap = 1, padding = 1 },
          #{ section = "recent_files", gap = 1, padding = 1 },
          sections = lib.generators.mkLuaInline ''
            {
              { section = "header" },
              { section = "keys", gap = 1, padding = 1 },
              { icon = " ", title = "Projects", section = "projects", indent = 2, padding = 1 },
              { title = "Sessions", section = "session", indent = 2, padding = 1 },
            }
          '';
        };
        picker.enabled = true;
        bigfile.enabled = true;
        quickfile.enabled = true;
        indent.enabled = true;
        words.enabled = true;
        # keys = [
        #   {
        #     key = "<leader><space>";
        #     mode = ["n"];
        #     lua = true;
        #     action = ''
        #       function() Snacks.picker.smart() end
        #     '';
        #     desc = "Smart Find Files";
        #   }
        # ];
      };
    };

    extraPackages = with pkgs; [
      ripgrep
      kitty
    ];

    # TODO: Don't make enter select_and_accept completion ever, just use Tab or M-Space to select
    # Enter is for new line (in file) or <CR> in cmdline
    # TODO: Unify cmdline and file-based keybindings
    # blink-cmp autocompletion plugion
    autocomplete.blink-cmp = {
      enable = true;
      setupOpts = {
        signature.enabled = true;
        cmdline.enabled = true;

        # Completion trigger on Alt-Space; Ctrl-Space is reserved for tmux prefix
        keymap = {
          preset = "default";
          "<C-space>" = [ "fallback" ];
          "<M-space>" = [
            "show"
            "show_documentation"
            "hide_documentation"
          ];
        };

        # Select and accept with `<M-space>`, close with `<C-e>`
        cmdline.keymap = {
          preset = "default";
          # TODO: Tab select_and_accept doesn't work, not sure why
          "<Tab>" = [
            "show"
            "accept"
          ];
          "<M-space>" = [
            "select_and_accept"
            "fallback"
          ];
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
      # nvf calls `telescope.load_extension('<name>')` in telescope's own
      # after-hook, so no VimEnter autocmd needed. `packages` is empty because
      # persisted-nvim is already in runtimepath via extraPlugins.
      extensions = [
        {
          name = "persisted";
        }
      ];
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
            "q" = "close_window";
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
              kind = [
                "shell_out"
                "shell_err"
                "shell_ret"
              ];
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

    # TODO: Contribute manually build Vim plugins to nixpkgs for auto-updates
    extraPlugins = {
      # Preserve proportional window sizes when the outer terminal (Ghostty)
      # resizes, so :terminal splits don't get squashed while regular buffers
      # take up all the reclaimed space.
      # Default register triggers (WinEnter/BufWinEnter). Avoid WinResized —
      # it also fires during the outer Ghostty resize, so bufresize ends up
      # registering the transient squashed layout and pins terminals at
      # whatever width the mid-tile redistribution produced.
      bufresize = {
        package = pkgs.vimPlugins.bufresize-nvim;
        setup = ''
          require('bufresize').setup()
        '';
      };

      auto-dark-mode = {
        package = pkgs.vimUtils.buildVimPlugin {
          pname = "auto-dark-mode.nvim";
          version = "main";
          src = pkgs.fetchFromGitHub {
            owner = "f-person";
            repo = "auto-dark-mode.nvim";
            rev = "e300259ec777a40b4b9e3c8e6ade203e78d15881";
            hash = "sha256-PhhOlq4byctWJ5rLe3cifImH56vR2+k3BZGDZdQvjng=";
          };
        };
        setup = ''
          -- solarized.nvim's lualine theme doesn't define `terminal` (falls
          -- back to normal/blue). Override the entry to cyan.
          local function with_terminal(theme_name)
            local theme = require('lualine.themes.' .. theme_name)
            theme.terminal = { a = { fg = '#002b36', bg = '#2aa198', gui = 'bold' } }
            return theme
          end
          -- auto-dark-mode only sets vim.opt.background; re-run `:colorscheme`
          -- so solarized.nvim re-applies its palette-dependent highlights
          -- (ColorScheme also fires, so our Diff* overrides re-apply too).
          local function apply(bg)
            vim.api.nvim_set_option_value('background', bg, {})
            vim.cmd.colorscheme('solarized')
            require('lualine').setup({ options = { theme = with_terminal('solarized') } })
          end
          require('auto-dark-mode').setup({
            set_dark_mode = function() apply('dark') end,
            set_light_mode = function() apply('light') end,
          })
        '';
      };
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
            async = true,
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
      # Persistent sessions per cwd, auto-restored on nvim startup
      # Pairs with tmux-resurrect: resurrect preserves the pane's cwd,
      # nvim relaunches there, persisted autoloads the matching session
      persisted-nvim = {
        package = pkgs-unstable.vimPlugins.persisted-nvim;
        setup = ''
          -- Autoload/autostart only inside boxvim ($BOXVIM set by the wrapper).
          -- Host nvim doesn't auto-source session files (no poisoning vector
          -- if a sandbox-written session ever got malicious) and doesn't
          -- auto-write them (no clobbering boxvim's state from quick host
          -- edits). Sessions still work manually via :Persisted load/start.
          --
          -- save_dir defaults to stdpath('data')/sessions/, which on both host
          -- and boxvim resolves to /home/sam/.local/share/nvf/sessions/ (the
          -- boxvim wrapper bind-mounts it so sessions persist across sandboxes
          -- and are portable between the two environments).
          local in_boxvim = vim.env.BOXVIM == "1"
          require("persisted").setup({
            save_dir = vim.fn.stdpath("data") .. "/sessions/",
            autoload = in_boxvim,
            autostart = in_boxvim,
            use_git_branch = false,
          })
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
    # Bug: When adding a dependency and running `lake update`, if I do `import <dep>` in a Lean file then Lean.nvim runs `lake setup-file` which will clone and attempt to build the dependency. However, this fails with:
    # ```
    # info: stderr:
    # /nix/store/qla374n3avx7nzaw2kvq6wj9y4agiw1l-clang-19.1.7/bin/clang: symbol lookup error: /nix/store/qla374n3avx7nzaw2kvq6wj9y4agiw1l-clang-19.1.7/bin/clang: undefined symbol: _ZNK4llvm3opt3Arg11getAsStringB5cxx11ERKNS0_7ArgListE, version LLVM_19.1
    # ```
    # which means Lean.nvim is using the wrong clang or doesn't have the right libraries loaded from `/nix/store/<hash>-lean/bin/clang`. Furthermore after this error the whole Neovim UI is unresponsive half the time and the terminal buffer stops working. Idk the fix but a workaround is to close the infoview window (`,i` to toggle) so Neovim works again, then run `lake build` which will succeed, then reopen the infoview and restart the Lean server for that file.
    lazy.plugins = {
      "lean.nvim" = {
        package = pkgs-unstable.vimPlugins.lean-nvim;
        setupModule = "lean";
        setupOpts = {
          mappings = true;
          # Narrower than upstream's 1/3 default — still comfortable for
          # goal state / hypotheses while leaving more room for code.
          # Pairs with the `leaninfo` FileType autocmd below, which calls
          # bufresize.resize_open() so the remaining width is redistributed
          # proportionally across existing splits instead of smushing the
          # rightmost one.
          infoview.width = 0.25;
          # Don't auto-open: lean.nvim is lazy-loaded by BufReadPre *.lean,
          # so on session restore the infoview opens mid-layout — before
          # the session script's `vert Nresize` commands finish — and
          # `Infoview.__width` gets frozen against an interim
          # `vim.o.columns`, leaving it stuck at ~minimum width. Toggle
          # manually with <localleader>i.
          infoview.autoopen = false;
        };
        event = [
          "BufReadPre *.lean"
          "BufNewFile *.lean"
        ];
        keys = [
          # Restart Lean LSP in case it gets in a bad build state after `lake build` in the terminal
          {
            key = "<localleader>R";
            mode = [ "n" ];
            lua = true;
            action = ''
              function()
                for _, client in ipairs(vim.lsp.get_clients()) do
                  if client.name == "lean" then
                    client.stop()
                  end
                end
                vim.cmd("edit")
              end
            '';
            desc = "Restart Lean LSP completely";
          }
        ];
      };
      # Magit-like git UI: status, commit/rebase/merge popups, worktree popup.
      # Routes diffs through diffview.nvim (already enabled above) for proper
      # side-by-side review.
      neogit = {
        package = pkgs-unstable.vimPlugins.neogit;
        setupModule = "neogit";
        setupOpts = {
          integrations = {
            diffview = true;
            telescope = true;
            snacks = true;
          };
          # Open status as a vertical split to the left so it composes with
          # existing layouts instead of taking the whole window
          kind = "vsplit";
          disable_commit_confirmation = false;
        };
        cmd = [
          "Neogit"
          "NeogitCommit"
          "NeogitLogCurrent"
          "NeogitResetState"
        ];
        keys = [
          {
            key = "<leader>gs";
            mode = [ "n" ];
            action = "<cmd>Neogit<cr>";
            desc = "Neogit status";
          }
          {
            key = "<leader>gw";
            mode = [ "n" ];
            action = "<cmd>Neogit worktree<cr>";
            desc = "Neogit worktree popup";
          }
          {
            key = "<leader>gc";
            mode = [ "n" ];
            action = "<cmd>Neogit commit<cr>";
            desc = "Neogit commit popup";
          }
          {
            key = "<leader>gl";
            mode = [ "n" ];
            action = "<cmd>Neogit log<cr>";
            desc = "Neogit log popup";
          }
          {
            key = "<leader>gp";
            mode = [ "n" ];
            action = "<cmd>Neogit push<cr>";
            desc = "Neogit push popup";
          }
        ];
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
      setupOpts = {
        multiplexer_integration = "tmux";
        # No-wrap: hitting C-l in the rightmost split (etc.) is a no-op
        # rather than wrapping back to the leftmost. Matches the tmux-side
        # no-wrap guards so the whole nav chain stops at outer edges.
        at_edge = "stop";
      };
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

    # External-write handling for agentic Claude workflows.
    # autoread: reload buffers whose files change externally without prompting.
    # updatetime: default 4000ms is too slow for CursorHold-based reload
    # detection; 250ms also makes LSP hovers/diagnostics feel snappier.
    options.autoread = true;
    options.updatetime = 250;

    # Show dots for trailing whitespace and non-breaking spaces
    options.list = true;
    options.listchars = "trail:·,nbsp:·,tab:  ";

    mini.bufremove = {
      enable = true;
    };

    # Leader key
    globals.mapleader = " ";
    keymaps = [
      {
        key = "k";
        mode = [
          "n"
          "v"
          "x"
        ];
        action = "gk";
        # Executes command without displaying it on the command line
        silent = true;
        desc = "Scroll up a visual line";
      }
      {
        key = "j";
        mode = [
          "n"
          "v"
          "x"
        ];
        action = "gj";
        silent = true;
        desc = "Scroll down a visual line";
      }
      {
        key = "<leader><Tab>";
        mode = [
          "n"
          "v"
        ];
        action = ":b#<CR>";
        silent = true;
        # Description is shown by which-key on the leader popup
        desc = "Switch to most recent buffer";
      }
      {
        key = "<leader>b/";
        mode = [
          "n"
          "v"
        ];
        action = ":vnew<CR>";
        silent = true;
        desc = "New buffer split right";
      }
      {
        key = "<leader>b-";
        mode = [
          "n"
          "v"
        ];
        action = ":new<CR>";
        silent = true;
        desc = "New buffer split below";
      }
      # C-hjkl is reserved for smart-splits pane navigation (normal mode
      # only). In insert and terminal modes these keys would forward to
      # tmux via send-keys and cause display corruption in :terminal
      # buffers (e.g. Claude). Swallow them so the user has to leave
      # insert/terminal mode first before navigating.
      {
        key = "<C-h>";
        mode = [
          "i"
          "t"
        ];
        action = "<Nop>";
        silent = true;
        desc = "Disable pane-nav key in insert/terminal mode";
      }
      {
        key = "<C-j>";
        mode = [
          "i"
          "t"
        ];
        action = "<Nop>";
        silent = true;
        desc = "Disable pane-nav key in insert/terminal mode";
      }
      {
        key = "<C-k>";
        mode = [
          "i"
          "t"
        ];
        action = "<Nop>";
        silent = true;
        desc = "Disable pane-nav key in insert/terminal mode";
      }
      {
        key = "<C-l>";
        mode = [
          "i"
          "t"
        ];
        action = "<Nop>";
        silent = true;
        desc = "Disable pane-nav key in insert/terminal mode";
      }
      {
        # Deletes the buffer, prompting to save if changed
        # Based on https://github.com/folke/snacks.nvim/blob/main/lua/snacks/bufdelete.lua
        key = "<leader>bd";
        mode = [
          "n"
          "v"
        ];
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
        mode = [
          "n"
          "v"
        ];
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
        mode = [
          "n"
          "v"
        ];
        action = "<cmd>vsp | terminal<CR>";
        silent = true;
        desc = "Open terminal to the right";
      }
      {
        key = "<Esc>";
        mode = [ "t" ];
        action = "<C-\\><C-n>";
        silent = true;
        desc = "Exit terminal mode";
      }
      {
        key = "<leader>w/";
        mode = [
          "n"
          "v"
        ];
        action = ":vsp<CR>";
        silent = true;
        desc = "New window split right";
      }
      {
        key = "<leader>w-";
        mode = [
          "n"
          "v"
        ];
        action = ":sp<CR>";
        silent = true;
        desc = "New window split below";
      }
      {
        key = "<leader>wd";
        mode = [
          "n"
          "v"
        ];
        action = ":close<CR>";
        silent = true;
        desc = "Close window";
      }
      {
        key = "<Esc>";
        mode = [ "n" ];
        action = "<cmd>nohlsearch<CR>";
        silent = true;
        desc = "Turn off search highlighting";
      }
      {
        key = "<leader>t";
        mode = [
          "n"
          "v"
        ];
        action = ":Neotree<CR>";
        silent = true;
        desc = "Open filetree";
      }
      {
        key = "<leader>ut";
        mode = [ "n" ];
        action = ":UndotreeToggle<CR>";
        silent = true;
        desc = "Toggle Undo Tree";
      }
      {
        key = "<leader>sl";
        mode = [ "n" ];
        action = ":Persisted load<CR>";
        silent = true;
        desc = "Load session for the current directory";
      }
      {
        key = "<leader>sL";
        mode = [ "n" ];
        action = ":Persisted load_last<CR>";
        silent = true;
        desc = "Load most recent session (global)";
      }
      {
        key = "<leader>ss";
        mode = [ "n" ];
        action = ":Persisted select<CR>";
        silent = true;
        desc = "Select session to load";
      }
      {
        key = "<leader>sS";
        mode = [ "n" ];
        action = ":Persisted save<CR>";
        silent = true;
        desc = "Save current session now";
      }
      {
        key = "<leader>st";
        mode = [ "n" ];
        action = ":Persisted toggle<CR>";
        silent = true;
        desc = "Toggle session (load/start/stop)";
      }
      {
        key = "<leader>sq";
        mode = [ "n" ];
        action = ":Persisted stop<CR>";
        silent = true;
        desc = "Disable autosave for this session";
      }
      {
        key = "<leader>se";
        mode = [ "n" ];
        action = ":Persisted start<CR>";
        silent = true;
        desc = "Enable autosave for this session";
      }
      {
        key = "<leader>sD";
        mode = [ "n" ];
        action = ":Persisted delete_current<CR>";
        silent = true;
        desc = "Delete current session";
      }
      {
        key = "<leader>sd";
        mode = [ "n" ];
        action = ":Persisted delete<CR>";
        silent = true;
        desc = "Delete a session from a list";
      }
      # TODO: Snacks picker or telescope?
      {
        key = "<leader><space>";
        mode = [ "n" ];
        lua = true;
        action = ''
          function()
            Snacks.picker.smart()
          end
        '';
        silent = true;
        desc = "Smart Find Files";
      }
      {
        key = "<leader>FB";
        mode = [ "n" ];
        lua = true;
        action = ''
          function() Snacks.picker.buffers() end
        '';
        desc = "Buffers";
      }
      {
        key = "<leader>FF";
        mode = [ "n" ];
        lua = true;
        action = ''
          function() Snacks.picker.files() end
        '';
        desc = "Find Files";
      }
      {
        key = "<leader>FP";
        mode = [ "n" ];
        lua = true;
        action = ''
          function() Snacks.picker.projects() end
        '';
        desc = "Projects";
      }
    ];

    binds.whichKey = {
      enable = true;
      register = {
        "<leader>b" = "+Buffers";
        "<leader>g" = "+Git";
        "<leader>s" = "+Sessions";
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
      # Canonical output (same AST → same format) for minimal, stable diffs.
      # Alejandra 4.0 preserves input line breaks, so multi-line lists stay
      # multi-line even if they'd fit on one — Claude edits and hand-written
      # expansions both lock in. nixfmt (RFC 166) is pure: always expands
      # multi-item lists, no input-layout memory.
      format = {
        enable = true;
        type = [ "nixfmt" ];
      };
      lsp = {
        enable = true;
      };
    };

    autocmds = [
      {
        enable = true;
        desc = "Tabs into 2 spaces for Nix";
        event = [ "FileType" ];
        pattern = [ "nix" ];
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
        # lean.nvim opens the infoview with `botright Xvsplit`, which steals
        # width from the adjacent window only. With 3+ columns this squashes
        # the rightmost one while the others stay put. bufresize's
        # resize_open() re-reads the registered pre-split layout and applies
        # proportional widths to every existing split, leaving the new
        # infoview at its configured width. vim.schedule defers the call
        # until after lean finishes its layout operations.
        desc = "Proportionally rebalance splits when Lean infoview opens (instead of squashing the rightmost)";
        event = [ "FileType" ];
        pattern = [ "leaninfo" ];
        callback = lib.generators.mkLuaInline ''
          function()
            vim.schedule(function()
              pcall(require('bufresize').resize_open)
            end)
          end
        '';
      }
      {
        enable = true;
        desc = "Trigger :checktime so buffers reload after external writes (Claude auto-accept, git pull, etc.)";
        event = [
          "BufEnter"
          "FocusGained"
          "CursorHold"
          "CursorHoldI"
        ];
        pattern = [ "*" ];
        command = "if mode() !~ '[cC]' | checktime | endif";
      }
      {
        enable = true;
        desc = "Format and save after external writes so Claude's auto-accept edits match project style";
        event = [ "FileChangedShellPost" ];
        pattern = [ "*" ];
        # Route through conform so conform-registered formatters (alejandra
        # for nix) win; fall back to the LSP for filetypes without a conform
        # formatter. vim.lsp.buf.format directly would skip conform and pick
        # up whatever the LSP ships with (e.g. nil's built-in nixfmt),
        # producing a different style from :w.
        callback = lib.generators.mkLuaInline ''
          function(args)
            local conform = require("conform")
            local has_fmt = #conform.list_formatters(args.buf) > 0
              or #vim.lsp.get_clients({bufnr = args.buf, method = "textDocument/formatting"}) > 0
            if not has_fmt then return end
            conform.format({bufnr = args.buf, async = false, lsp_format = "fallback"})
            vim.api.nvim_buf_call(args.buf, function() vim.cmd.write() end)
          end
        '';
      }
      {
        enable = true;
        desc = "Enter insert mode when starting terminal";
        event = [ "TermOpen" ];
        callback = lib.generators.mkLuaInline ''
          function()
            vim.cmd("startinsert")
          end
        '';
      }
      {
        enable = true;
        desc = "Enter insert mode and restore guicursor in terminal after Git buffer";
        event = [ "BufLeave" ];
        pattern = [
          "*COMMIT_EDITMSG"
          "*git-rebase-todo"
        ];
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
      # {
      #   enable = true;
      #   desc = "Disable Noice before loading session";
      #   event = ["User"];
      #   pattern = ["PersistedLoadPre"];
      #   callback = lib.generators.mkLuaInline ''
      #     function()
      #       vim.cmd("Noice disable")
      #     end
      #   '';
      # }
      # {
      #   enable = true;
      #   desc = "Enable Noice after loading session";
      #   event = ["User"];
      #   pattern = ["PersistedLoadPost"];
      #   callback = lib.generators.mkLuaInline ''
      #     function()
      #       vim.cmd("Noice enable")
      #     end
      #   '';
      # }
      # Sidebar-style buffers (neo-tree, undotree, Neogit*, Diffview*) don't
      # round-trip through :mksession — the session would restore them as
      # broken empty buffers. Pattern from persisted.nvim's README §Events.
      # Neogit* covers NeogitStatus, NeogitPopup, NeogitLogView, NeogitDiffView,
      # NeogitCommitView, NeogitStashView, NeogitRefsView, NeogitConsole,
      # NeogitReflogView, NeogitCommitSelectView, NeogitGitCommandHistory.
      # Diffview* covers DiffviewFiles (file panel) and DiffviewFileHistory.
      #
      # Also strips non-file buffers other than :terminal: snacks dashboard,
      # snacks scratch, quickfix, help, prompt. These carry no recoverable
      # state — on reload they'd reappear as empty windows and hide the
      # files we actually care about. :terminal buffers are kept; nvim's
      # sessionoptions include "terminal" by default, so the saved session
      # reopens them as fresh shells in the original window layout.
      {
        enable = true;
        desc = "Strip sidebar + non-file buffers (keep :terminal) before persisted save";
        event = [ "User" ];
        pattern = [ "PersistedSavePre" ];
        callback = lib.generators.mkLuaInline ''
          function()
            local strip = { "neo-tree", "undotree", "diff", "leaninfo" }
            for _, buf in ipairs(vim.api.nvim_list_bufs()) do
              local ft = vim.bo[buf].filetype
              local bt = vim.bo[buf].buftype
              if (bt ~= "" and bt ~= "terminal")
                or vim.tbl_contains(strip, ft)
                or ft:match("^Neogit")
                or ft:match("^Diffview")
              then
                vim.api.nvim_buf_delete(buf, { force = true })
              end
            end
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

    languages.markdown = {
      enable = true;
      format = {
        enable = true;
      };
      lsp = {
        enable = true;
      };
      extensions = {
        render-markdown-nvim = {
          enable = true;
          setupOpts = {
            anti_conceal = {
              ignore = {
                code_inline = true;
              };
            };
            # Multiple `==` on the same line even within code blocks
            # cause the whole line to highlight and conceal the `==` signs
            # Switch to removing all highlights if desired
            # inline_highlight = {
            #   enabled = false;
            # };
          };
        };
      };
    };
    # TODO: Modeline icons and general nerd font support (already installed in home.nix and supported by Ghostty)
    #utility.icon-picker.enable = true;

    luaConfigRC.config-dir = ''
      require("config")
    '';

    # Route :terminal-launched editor invocations (git commit, fzf, etc.)
    # back through flatten.nvim. system.nix sets a system-wide EDITOR=vim
    # which spawns the real vim binary — flatten is nvim-only, so the
    # child ignores the parent's NVIM socket and a full TUI opens in the
    # terminal pane. vim.env scopes the override to this nvim and its
    # subprocesses, leaving the system-level EDITOR untouched.
    #
    # The serverstart fallback covers the rare case where the auto-server
    # didn't come up at boot (e.g. the wrapper's $NVIM_LISTEN_ADDRESS
    # collided with a stale socket). Without v:servername, :terminal
    # can't export $NVIM and flatten opens nested nvim on git commit.
    luaConfigRC.editor-env = ''
      vim.env.EDITOR = "nvim"
      if vim.v.servername == "" then
        pcall(vim.fn.serverstart)
      end
    '';

  };
}
