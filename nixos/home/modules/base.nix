# Universal home-manager base — imported by every host entry. Contains
# shell, tmux, CLI tools, git, and other
# TTY-friendly configs that work on both NixOS workstations and a remote
# Ubuntu box. GUI bits live in ./gnome.nix; NixOS-only bits (rebuild
# wrapper) live in ./alias.nix. home.username / homeDirectory are derived
# from the `username` specialArg here, uniform across every host.
{
  inputs,
  pkgs,
  pkgs-unstable,
  lib,
  config,
  username,
  ...
}:
{

  home.username = username;
  home.homeDirectory = "/home/${username}";

  home.packages = with pkgs; [
    # Bencher CLI — built from upstream's flake (see flake.nix `bencher` input).
    inputs.bencher.packages.${pkgs.stdenv.hostPlatform.system}.default
    ripgrep
    htop
    jq
    sesh
    fzf
    # Used by the sesh-picker ctrl-f "find" tab below
    fd
    # nvim-which-key-style bindings viewer for tmux. Shows every binding
    # (user-added and tmux defaults): `-a` includes un-noted entries and
    # `-N` uses the note for noted ones, falling back to the command.
    # Trusts tmux's built-in column padding rather than reformatting —
    # our own `column -t` reflow broke alignment when a note had enough
    # internal whitespace to look like a column break. Piped through
    # `less` so long lists scroll and `q` quits.
    (pkgs.writeShellScriptBin "tmux-which-key-all" ''
      set -euo pipefail
      tmux list-keys -aN 2>/dev/null | sort | ${pkgs.less}/bin/less -R
    '')
    # sesh session-switcher popup invoked by `prefix + o` in tmux. Kept as a
    # shell script because the fzf binding flags are too ugly to embed
    # inline in the tmux config.
    (pkgs.writeShellScriptBin "sesh-picker" ''
      sel=$(
        sesh list --icons | fzf-tmux -p 80%,70% \
          --no-sort --ansi --border-label ' sesh ' --prompt '⚡  ' \
          --header '  ^a all ^t tmux ^g configs ^x zoxide ^d tmux kill ^f repos' \
          --bind 'tab:down,btab:up' \
          --bind 'ctrl-a:change-prompt(⚡  )+reload(sesh list --icons)' \
          --bind 'ctrl-t:change-prompt(🪟  )+reload(sesh list -t --icons)' \
          --bind 'ctrl-g:change-prompt(⚙️  )+reload(sesh list -c --icons)' \
          --bind 'ctrl-x:change-prompt(📁  )+reload(sesh list -z --icons)' \
          --bind 'ctrl-f:change-prompt(🔎  )+reload(fd -H -d 6 -E clones "^\\.git$" ~/repos -x dirname)' \
          --bind 'ctrl-d:execute(tmux kill-session -t {2..})+change-prompt(⚡  )+reload(sesh list --icons)' \
          --preview-window 'right:55%' \
          --preview 'sesh preview {}'
      )
      # Guard against Esc/Ctrl+c cancel: fzf returns empty string, which
      # would otherwise reach `sesh connect` and hang the popup.
      [ -z "$sel" ] && exit 0
      exec sesh connect "$sel"
    '')
  ];

  # sesh config — declarative. Wildcard rule means every directory under
  # ~/repos (project roots AND worktrees) connects with `nvim`
  # as the startup command. Additional explicit [[session]] entries below
  # for SSH remotes.
  home.file.".config/sesh/sesh.toml".text = ''
    #:schema https://github.com/joshmedeski/sesh/raw/main/sesh.schema.json

    [[wildcard]]
    pattern = "~/repos/**"
    startup_command = "nvim"

    # SSH remote sessions — uncomment and fill in:
    # [[session]]
    # name = "prod-box"
    # startup_command = "ssh prod-box"
  '';

  # tmux-which-key: replace the plugin's default menu with our own groups
  # (Sessions, Windows, Panes). Menu opens via `prefix + Space` (plugin
  # default). Item hotkeys use `x` for destructive actions, matching tmux
  # defaults. Item commands mirror the direct bindings in extraConfig; the
  # menu is the discoverable surface, direct bindings are muscle memory.
  home.file.".config/tmux/plugins/tmux-which-key/config.yaml".text = ''
    command_alias_start_index: 200
    keybindings:
      prefix_table: Space
    title:
      style: align=centre,bold
      prefix: tmux
      prefix_style: fg=green,align=centre,bold
    position:
      x: C
      y: C
    # Required by build.py even when unused.
    custom_variables: {}
    macros: []
    items:
      # Top-level leaves: most common tmux defaults one keystroke away,
      # plus our sesh-last shortcut. Duplicated in the groups below for
      # drill-in discovery — either path works.
      - name: Last session
        key: Tab
        # Double quotes (not single) so build.py's outer single-quoted
        # wrapper doesn't break on a nested `'`.
        command: run-shell "sesh last"
      - name: Detach
        key: d
        command: detach-client
      - name: Copy mode
        key: "["
        command: copy-mode
      - name: Run tmux command
        key: ":"
        command: command-prompt
      - separator: true
      - name: +Sessions
        key: s
        menu:
          - name: Sesh picker
            key: s
            command: run-shell sesh-picker
          - name: New
            key: c
            command: command-prompt -p "New session name:" "new-session -s '%%'"
          - name: Prev
            key: p
            command: switch-client -p
          - name: Next
            key: n
            command: switch-client -n
          - name: Kill current
            key: x
            command: confirm -p "Kill session? (y/N):" kill-session
          - name: Rename
            key: r
            command: command-prompt -I "#S" "rename-session -- %%"
          - name: Detach
            key: d
            command: detach-client
      - name: +Windows
        key: w
        menu:
          - name: New at cwd
            key: c
            command: new-window -c "#{pane_current_path}"
          - name: Next
            key: n
            command: next-window
          - name: Prev
            key: p
            command: previous-window
          - name: Kill
            key: x
            command: kill-window
          - name: Rename
            key: r
            command: command-prompt -I "#W" "rename-window -- %%"
          - name: Choose (tree picker)
            key: w
            command: choose-window -Z
      - name: +Panes
        key: p
        menu:
          - name: Split vertical
            key: "/"
            command: split-window -h -c "#{pane_current_path}"
          - name: Split horizontal
            key: "-"
            command: split-window -v -c "#{pane_current_path}"
          - name: Kill
            key: x
            command: kill-pane
          - name: +Swap
            key: s
            menu:
              - name: Swap left
                key: h
                command: swap-pane -t {left-of}
              - name: Swap down
                key: j
                command: swap-pane -t {bottom-of}
              - name: Swap up
                key: k
                command: swap-pane -t {top-of}
              - name: Swap right
                key: l
                command: swap-pane -t {right-of}
  '';

  # Regenerate tmux-which-key's init.tmux from our config.yaml on every
  # home-manager activation. We can't rely on the plugin's own runtime
  # build (plugin.sh.tmux's build.py shebang points at a bare python3 in
  # the Nix store that lacks pyyaml — see the disable-autobuild note on
  # the plugin entry). Instead, run build.py with our own python3 that
  # includes pyyaml.
  home.activation.tmuxWhichKeyBuild = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    py='${pkgs.python3.withPackages (ps: [ ps.pyyaml ])}/bin/python3'
    build='${pkgs-unstable.tmuxPlugins.tmux-which-key}/share/tmux-plugins/tmux-which-key/plugin/build.py'
    cfg="$HOME/.config/tmux/plugins/tmux-which-key/config.yaml"
    init="$HOME/.local/share/tmux/plugins/tmux-which-key/init.tmux"
    $DRY_RUN_CMD mkdir -p "$(dirname "$init")"
    # Remove any stale init.tmux (plugin's first run created one with
    # restrictive 0600 perms that build.py can't open with 'w+').
    $DRY_RUN_CMD rm -f "$init"
    $DRY_RUN_CMD "$py" "$build" "$cfg" "$init"
  '';

  programs.bash = {
    enable = true;
    # Changes backup files `ls` color to dim cyan, otherwise they are invisible with solarized dark theme
    # Add Rust binaries to path, so `cargo install` works OOTB
    bashrcExtra = ''
      LS_COLORS=$(echo "$LS_COLORS" | sed 's/=00;90/=36;2/g')
      export PATH="$HOME/.cargo/bin:$PATH"
      # sops-decrypted tokens, exported on hosts that hold the age key.
      # GH_TOKEN drives `gh api`; NIX_CONFIG carries the access-tokens line
      # for private flake inputs. The dev microvm has no key of its own and
      # no secret files — `ssh dev-vm` forwards both from the host session
      # (see SendEnv/AcceptEnv), so these reads simply no-op inside the VM.
      [ -r /run/secrets/gh-token ] && export GH_TOKEN="$(cat /run/secrets/gh-token)"
      [ -r /run/secrets/rendered/nix-access-tokens ] && export NIX_CONFIG="$(cat /run/secrets/rendered/nix-access-tokens)"
      # AWS creds for Terraform / aws CLI — the WRITE pair, host-only. Same
      # guard: a no-op on hosts without these secret files (the ubuntu bench
      # box, the microvm). The microvm never receives these; the ssh-dev-vm
      # wrapper forwards a separate read-only pair instead (see gui.nix), so a
      # compromised VM session can read infra state but not mutate it.
      [ -r /run/secrets/aws-access-key-id ] && export AWS_ACCESS_KEY_ID="$(cat /run/secrets/aws-access-key-id)"
      [ -r /run/secrets/aws-secret-access-key ] && export AWS_SECRET_ACCESS_KEY="$(cat /run/secrets/aws-secret-access-key)"
      # Bencher CLI API key — forwarded into the dev microvm like GH_TOKEN.
      [ -r /run/secrets/bencher-key ] && export BENCHER_API_KEY="$(cat /run/secrets/bencher-key)"
      # GCP disabled for now — re-enable with the gcp-credentials secret in host.nix.
      # [ -r /run/secrets/gcp-credentials ] && export GOOGLE_APPLICATION_CREDENTIALS=/run/secrets/gcp-credentials
      # Ignore C-d at an empty prompt so a misclick doesn't exit bash (and
      # close Ghostty). C-Shift-w is the intentional close shortcut.
      set -o ignoreeof
      # Bare `tmux` (outside an existing session) defaults to attaching
      # or creating a session named after the current directory, so
      # `prefix + s` shows real names instead of `0`/`1`/... Passes
      # through unchanged when args are given (e.g. `tmux new-session -s
      # foo`, `tmux kill-server`), and when already inside a session.
      tmux() {
        if [ $# -eq 0 ] && [ -z "$TMUX" ]; then
          command tmux new-session -A -s "$(basename "$PWD")"
        else
          command tmux "$@"
        fi
      }
    '';
  };

  # TODO: cd'ing into a Nix flake dir with Direnv enabled often doesn't show the full prompt (e.g. Rust version) till running another command
  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    # Catppuccin Mocha palette — Starship's defaults use ANSI names
    # ("red", "green", "blue"). Defining a palette with matching keys
    # overrides those names to Catppuccin hex codes, so the prompt
    # unifies with Ghostty's Catppuccin Mocha theme without rewriting
    # each module's format string. Palette values verbatim from
    # github.com/catppuccin/starship.
    settings = {
      palette = "catppuccin_mocha";
      palettes.catppuccin_mocha = {
        rosewater = "#f5e0dc";
        flamingo = "#f2cdcd";
        pink = "#f5c2e7";
        mauve = "#cba6f7";
        red = "#f38ba8";
        maroon = "#eba0ac";
        peach = "#fab387";
        yellow = "#f9e2af";
        green = "#a6e3a1";
        teal = "#94e2d5";
        sky = "#89dceb";
        sapphire = "#74c7ec";
        blue = "#89b4fa";
        lavender = "#b4befe";
        text = "#cdd6f4";
        subtext1 = "#bac2de";
        subtext0 = "#a6adc8";
        overlay2 = "#9399b2";
        overlay1 = "#7f849c";
        overlay0 = "#6c7086";
        surface2 = "#585b70";
        surface1 = "#45475a";
        surface0 = "#313244";
        base = "#1e1e2e";
        mantle = "#181825";
        crust = "#11111b";
      };
    };
  };

  programs.direnv = {
    # mkDefault so a host can switch direnv off with a plain `enable = false`
    # (desktop.nix does, to keep .envrc/devshell auto-exec out of the
    # ~/repos tree the dev microvm shares).
    enable = lib.mkDefault true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  # zoxide tracks directories you cd into. sesh surfaces those in its
  # picker so any project you've recently touched is one fuzzy-search away.
  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.tmux = {
    enable = true;
    shortcut = "Space";
    mouse = true;
    terminal = "tmux-256color";
    keyMode = "vi";
    plugins = with pkgs.tmuxPlugins; [
      sensible
      resurrect
      continuum
      {
        # Catppuccin status bar. Options must be set BEFORE catppuccin.tmux
        # runs so the flavor and styling take effect; put them in the
        # per-plugin extraConfig (emitted by home-manager right before this
        # plugin's run-shell, see home-manager's programs/tmux.nix:138).
        plugin = catppuccin;
        extraConfig = ''
          set -g @catppuccin_flavor "mocha"
          set -g @catppuccin_window_status_style "rounded"
          # Default session module shows only current session (`#S`). Override
          # to list every session via a shell substitution; attached one
          # wrapped in brackets. Updates every `status-interval` (default 15s).
          set -g @catppuccin_session_text "#(tmux list-sessions -F '#{?session_attached,[#S],#S}' 2>/dev/null | tr '\n' ' ')"
        '';
      }
      {
        # Two workarounds for nixpkgs packaging bugs, both required:
        # 1. XDG mode — the plugin's default is to `cp config.example.yaml`
        #    into its own /nix/store path (read-only); XDG mode redirects
        #    writes to $XDG_CONFIG_HOME and $XDG_DATA_HOME.
        # 2. Disable autobuild — `build.py`'s shebang points at a bare
        #    python3 store path lacking pyyaml, so the rebuild step crashes
        #    under `set -e` and the plugin never reaches `source-file` at
        #    the end. Skipping autobuild uses the static init.example.tmux
        #    as-is, which has all default bindings. Customizing config.yaml
        #    won't take effect until build.py runs manually (out of scope).
        # Must be set BEFORE the plugin's run-shell, hence in this entry's
        # extraConfig rather than programs.tmux.extraConfig (which
        # home-manager appends AFTER all run-shell lines).
        plugin = pkgs-unstable.tmuxPlugins.tmux-which-key;
        extraConfig = ''
          set -g @tmux-which-key-xdg-enable 1
          set -g @tmux-which-key-disable-autobuild 1
        '';
      }
    ];
    extraConfig = ''
      set -ga terminal-overrides ",*256col*:Tc"
      # Ghostty's terminfo is `xterm-ghostty`, which doesn't match the
      # `*256col*` pattern above — without this, truecolor passes from
      # Ghostty *to* tmux but not *through* tmux to inner TUIs.
      set -as terminal-features ",xterm-ghostty:RGB"
      set -g renumber-windows on
      # Windows and panes start at 1 instead of 0 — matches the number-row
      # keys (`prefix 1`, `prefix 2`, ...) used to jump between them, so
      # the leftmost window is always `prefix 1`.
      set -g base-index 1
      setw -g pane-base-index 1

      # Let OSC escape sequences (e.g. OSC 9 desktop notifications from
      # Claude Code) reach the outer terminal instead of tmux swallowing
      # them. Required for Ghostty → GNOME notifications to fire from
      # inside tmux.
      set -g allow-passthrough on

      # `keyMode = "vi"` above covers copy-mode navigation (h/j/k/l/w/b/e,
      # /, ?, n/N, etc.). tmux's vi mode doesn't bind `v` or `y` though, so
      # add them: v begins selection, y yanks via wl-copy so the result
      # lands in the Wayland clipboard (copy-pipe-and-cancel also keeps it
      # in tmux's paste buffer and exits copy-mode). Enter copy-mode with
      # `prefix + [`, paste with `prefix + ]`. MouseDragEnd1Pane routes
      # mouse drag-release through the same pipe so plain drag-select
      # respects pane boundaries and lands in the system clipboard;
      # Shift-drag still bypasses tmux entirely for terminal-native
      # selection across panes.
      bind -T copy-mode-vi v send-keys -X begin-selection
      bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel 'wl-copy'
      bind -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel 'wl-copy'

      # Jump between shell prompts in copy-mode, anchored on OSC 133
      # semantic-prompt markers emitted by Ghostty's shell integration.
      bind -T copy-mode-vi [ send-keys -X previous-prompt
      bind -T copy-mode-vi ] send-keys -X next-prompt

      # Every custom binding's -N note is prefixed with "» " so it's
      # easy to spot our own entries in the (now all-bindings) viewer
      # popup. See home.packages above for the viewer.
      bind -N "» new window at current pane's cwd" c new-window -c "#{pane_current_path}"

      # Idiomatic pane splits (like many modern TUIs / neo-tree). `/` for a
      # vertical split (pane to the right), `-` for a horizontal split
      # (pane below); both inherit the active pane's cwd. Displaces the
      # defaults: `/` was describe-key (moved to `!` below), `-` was
      # delete-buffer (dropped — use `prefix :` + `delete-buffer` if
      # needed). Default `%` and `"` stay bound for muscle memory.
      bind -N "» split pane right" / split-window -h -c "#{pane_current_path}"
      bind -N "» split pane below" - split-window -v -c "#{pane_current_path}"
      # `!` was break-pane; reassigned to describe-key since `/` is now
      # the split binding. Break-pane is still reachable via `prefix :
      # break-pane` if needed.
      bind -N "» describe key binding" '!' command-prompt -kpkey "list-keys -1N \"%%%\""

      # smart-splits.nvim-style C-h/j/k/l. Uses the `@pane-is-vim` pane
      # option that smart-splits sets on init, instead of the ps/comm regex
      # in vim-tmux-navigator. The regex approach fails for our mnw-wrapped
      # nvim (comm=`.nvim-wrapped`), so tmux never forwards C-h to nvim and
      # within-nvim splits don't navigate.
      #
      # No-wrap variant: guard `select-pane` with `pane_at_<edge>` so pressing
      # e.g. C-l in the rightmost pane is a no-op instead of wrapping back to
      # the leftmost pane. Wrapping surfaced rendering corruption when focus
      # bounced to a nvim pane whose SIGWINCH had been missed.
      bind-key -N "» nav pane left (smart-splits)"  -n C-h if -F '#{@pane-is-vim}' 'send-keys C-h' "if -F '#{pane_at_left}'   '''   'select-pane -L'"
      bind-key -N "» nav pane down (smart-splits)"  -n C-j if -F '#{@pane-is-vim}' 'send-keys C-j' "if -F '#{pane_at_bottom}' '''   'select-pane -D'"
      bind-key -N "» nav pane up (smart-splits)"    -n C-k if -F '#{@pane-is-vim}' 'send-keys C-k' "if -F '#{pane_at_top}'    '''   'select-pane -U'"
      bind-key -N "» nav pane right (smart-splits)" -n C-l if -F '#{@pane-is-vim}' 'send-keys C-l' "if -F '#{pane_at_right}'  '''   'select-pane -R'"

      # Extended-keys plumbing for modern Ctrl+Shift+<letter> bindings.
      # Kept on (no bindings using it yet) because nvim benefits from it
      # independently — nvim negotiates its own keyboard protocol via
      # tmux passthrough and gains distinct C-S-<letter> handling that
      # way. Handy starting point when we want to re-add far-nav bindings.
      #
      # When wiring tmux-side bindings for these keys (e.g. C-S-h to jump
      # to `{left}` pane), Ghostty↔tmux negotiation has to actually land;
      # on last attempt Ghostty stayed in legacy mode for shell panes
      # (sending 0x08 for Ctrl+Shift+h) despite the settings below. Debug
      # with `tmux -vvvv` to see what escape sequence tmux sends at start,
      # and compare with Ghostty's supported protocols (CSI-u/fixterms
      # first-class, modifyOtherKeys partial, kitty keyboard).
      set -s extended-keys on
      set -s extended-keys-format csi-u
      set -as terminal-features 'xterm*:extkeys'

      # Alt-hjkl resize bindings — same smart-splits pattern as C-hjkl. If
      # nvim owns the pane, forward to nvim (which resizes between nvim
      # splits or calls back to tmux at the edge). Otherwise resize the
      # tmux pane directly. Step size 3 cells/rows per press.
      bind-key -N "» resize pane left (smart-splits)"  -n M-h if -F '#{@pane-is-vim}' 'send-keys M-h' 'resize-pane -L 3'
      bind-key -N "» resize pane down (smart-splits)"  -n M-j if -F '#{@pane-is-vim}' 'send-keys M-j' 'resize-pane -D 3'
      bind-key -N "» resize pane up (smart-splits)"    -n M-k if -F '#{@pane-is-vim}' 'send-keys M-k' 'resize-pane -U 3'
      bind-key -N "» resize pane right (smart-splits)" -n M-l if -F '#{@pane-is-vim}' 'send-keys M-l' 'resize-pane -R 3'

      # `prefix + w s h/j/k/l` swaps the current pane with its neighbor in
      # the given direction. Matches nvim smart-splits' `<leader>wsh/j/k/l`
      # muscle memory (smart-splits calls them "windows"; tmux calls them
      # "panes" — same concept, different naming). Uses key-tables for the
      # chord. This hijacks `prefix + w` (tmux default: choose-window),
      # but choose-window is still reachable via the which-key menu.
      bind-key -N "» pane swap chord (w-s-h/j/k/l)" w switch-client -T ws
      bind-key -T ws s switch-client -T ws-swap
      bind-key -N "» swap pane left"  -T ws-swap h swap-pane -t '{left-of}'
      bind-key -N "» swap pane down"  -T ws-swap j swap-pane -t '{bottom-of}'
      bind-key -N "» swap pane up"    -T ws-swap k swap-pane -t '{top-of}'
      bind-key -N "» swap pane right" -T ws-swap l swap-pane -t '{right-of}'

      # sesh session picker (prefix+s, replacing tmux's default choose-tree).
      # Full fzf setup lives in the `sesh-picker` shell script in
      # home.packages: source-filter tabs (^a/^t/^g/^x/^f),
      # kill-from-picker (^d), preview pane. The script uses `fzf-tmux -p`
      # which opens its own tmux popup — wrapping this in `display-popup`
      # nests two popups and silently breaks, so we use run-shell instead.
      bind -N "» sesh session picker" s run-shell sesh-picker
      # Bounce between the two most-recently-attached sessions
      bind -N "» last session (sesh)" Tab run-shell "sesh last"
      # Kill the current session; with detach-on-destroy off, tmux stays in
      # the next session instead of quitting.
      bind -N "» kill current session" k confirm -p "Kill current session? (y/N):" kill-session
      bind -rN "» prev session" '(' switch-client -p\; refresh-client -S
      bind -rN "» next session" ')' switch-client -n\; refresh-client -S
      # Show my described bindings in a popup.
      bind -N "» show all bindings" '?' display-popup -E -w 70% -h 70% tmux-which-key-all

      # Recommended by sesh: closing a session leaves you attached to
      # another session rather than exiting tmux entirely.
      set -g detach-on-destroy off

      # Catppuccin status line modules. These reference @catppuccin_status_*
      # options that catppuccin.tmux populates when it runs, so these lines
      # must come AFTER the plugin's run-shell. home-manager emits the main
      # extraConfig last (via lib.mkAfter), so this block lands in the right
      # place. `#{E:...}` expands the referenced option.
      set -g status-right-length 100
      set -g status-left-length 100
      set -g status-left ""
      set -g status-right "#{E:@catppuccin_status_application}"
      set -agF status-right "#{E:@catppuccin_status_session}"

      set -g @continuum-restore 'on'
      set -g @continuum-save-interval '5'

      # tmux-assistant-resurrect: persist AI-assistant sessions across restarts.
      # The post-save hook records each pane's Claude session id (via the
      # SessionStart hook wired in claude.nix, keyed by the claude PID so two
      # conversations in one directory don't collide); the post-restore hook
      # relaunches `claude --resume <id>` in each restored pane. Assistants are
      # deliberately kept OUT of @resurrect-processes — the hooks own resuming,
      # and listing them there would instead start a bare session-less claude.
      set -g @resurrect-hook-post-save-all "bash '${inputs.tmux-assistant-resurrect}/scripts/save-assistant-sessions.sh'"
      set -g @resurrect-hook-post-restore-all "bash '${inputs.tmux-assistant-resurrect}/scripts/restore-assistant-sessions.sh'"
    '';
  };

  # nvim is the editor everywhere (the nvf package's vim/vi aliases resolve
  # to nvim too). Set it as the general editor in this shared HM base so it
  # holds in every interactive shell (hm-session-vars is sourced by the
  # login shell via programs.bash above). The system-level default also
  # points at nvim — common/base.nix's programs.neovim.defaultEditor — so
  # host, guest, and the user's shells all agree.
  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
  };

  programs.git = {
    enable = true;
    settings = {
      user = {
        name = "samuelburnham";
        email = "45365069+samuelburnham@users.noreply.github.com";
      };
      init.defaultBranch = "main";
      rerere.enabled = true;
      # Belt-and-suspenders over $EDITOR for git invocations that don't
      # source the shell env (hooks, GUI clients).
      core.editor = "nvim";
    };
  };

  programs.gh = {
    enable = true;
    gitCredentialHelper.enable = true;
  };

  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "25.05";
}
