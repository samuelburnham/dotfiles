# Universal home-manager base — imported by every host entry. Contains
# shell, tmux, CLI tools, worktrunk, git, claude-code, and other
# TTY-friendly configs that work on both NixOS workstations and a remote
# Ubuntu box. GUI bits live in ./gnome.nix; NixOS-only bits (rebuild
# wrapper) live in ./alias.nix. Each host sets its own home.username /
# homeDirectory; we avoid hardcoding them here.
{
  pkgs,
  pkgs-unstable,
  pkgs-master,
  inputs,
  lib,
  config,
  ...
}:
{
  imports = [
    inputs.worktrunk.homeModules.default
  ];

  # worktrunk — upstream's home-manager module installs the package and
  # wires `eval "$(wt config shell init bash)"` into bash.initExtra. Hooks
  # and aliases still live in ~/.config/worktrunk/config.toml below.
  programs.worktrunk = {
    enable = true;
    enableBashIntegration = true;
  };

  home.packages = with pkgs; [
    ripgrep
    htop
    jq
    # Pinned to nixpkgs master for fast-moving updates; bump via:
    #   nix flake update nixpkgs-master
    pkgs-master.claude-code
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

  # Claude Code settings — declarative replacement for a stale symlink
  # from an older home-manager generation. `package = null` skips the
  # module's own claude-code install since we already pull
  # pkgs-master.claude-code into home.packages above for newer releases.
  # Only the CLAUDE.md memory lives here — settings.json (permissions,
  # hooks, theme, sandbox mode) is rendered by apps/claude.nix on each
  # `nix run apps#claude`, so allow-list tweaks apply on the next launch
  # without a nixos-rebuild.
  programs.claude-code = {
    enable = true;
    package = null;
    memory.text = ''
      # Research clones

      `~/repos/clones/` contains third-party source trees cloned for read-only
      research and context — not for editing. Use them to inspect upstream
      implementations, cross-reference APIs, and answer "how does X actually
      work" questions instead of guessing from training data.

      Sorted by topic. New clones land in the matching topic dir.

      Rules:
      - Read-only. Don't edit, commit, or push here. If you need to modify
        upstream code, fork into `~/repos/forks/` instead.
      - Check here before WebFetching a repo's docs or source — the local
        clone is authoritative for whatever commit it's pinned to.
      - The clones aren't guaranteed up-to-date with upstream; if freshness
        matters, note the checked-out commit or fetch first.

      # Comments and docstrings

      Comments and docstrings explain the code, not the author's process.
      Never write a comment whose subject is you or this session — what you
      did, why you changed it, what it replaces, what task it came from, or
      which caller prompted it. That belongs in the commit message or PR
      description and rots the moment the code moves.

      A comment is only worth writing if a future reader — with no knowledge
      of this conversation — would benefit from it. It should explain:
      - *what* the code does, only when the code itself isn't self-evident
        (rare — prefer better names first); or
      - *why* the code is the way it is: a non-obvious constraint, a subtle
        invariant, a workaround for a specific upstream bug, behavior that
        would otherwise surprise a reader.

      Concretely, never write:
      - "Added X to fix Y", "Replaced the old Z", "Refactored from ..."
      - "Used by the foo flow", "Called from bar.ts", "Handles the case
        from issue #123"
      - Restatements of the code ("increment i by 1", "return the result")
      - TODOs referencing the current task ("TODO: wire this up once the
        other PR lands") — track those in the PR, not the source.

      This applies to every language's comment/docstring syntax (`//`, `#`,
      `/** */`, `"""..."""`, `---`, `;;`, etc.) and to commit messages for
      code *inside* diffs (the commit body itself is the right place for
      process narrative; the code is not).
    '';
  };

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

  # worktrunk — `wt switch --create <branch>` spins up a new worktree as a
  # sibling dir (default template) and fires this pre-start hook, which
  # creates a dedicated tmux session for it and drops the current client
  # into it. `wt remove` / `wt merge` reverse the whole thing.
  home.file.".config/worktrunk/config.toml".text = ''
    [pre-start]
    tmux = """
    # Worktrunk's default template creates sibling worktrees as
    # `<repo>.<branch>`, so basename yields e.g. `dotfiles.test`. tmux's
    # target-spec grammar always parses `.` as a `session.pane`
    # separator — even inside a `=exact` prefix — which is unfixable at
    # lookup time. Strip dots at the source so creation and lookup both
    # see a bare session name. `tr . -` also covers dotted branch names
    # like `v1.2.3`; worktrunk only sanitizes slashes, not dots.
    S=$(basename "{{ worktree_path }}" | tr . -)
    tmux new-session -d -s "$S" -c "{{ worktree_path }}"
    # Guarded on $TMUX so calls from outside tmux are a no-op instead
    # of erroring on "no current client".
    [ -n "$TMUX" ] && tmux switch-client -t "=$S"
    """
    # Guarded on .envrc presence so worktrees in non-direnv repos don't
    # error. `direnv allow` accepts a path and resolves the .envrc itself.
    direnv = """
    [ -f "{{ worktree_path }}/.envrc" ] && direnv allow "{{ worktree_path }}" || true
    """

    [pre-remove]
    tmux = """
    # Must match pre-start's transform so we target the session that
    # was actually created. Error-swallow preserved: kill-session is a
    # no-op if the session doesn't exist, and we don't want `wt remove`
    # to fail on cleanup of a missing session.
    S=$(basename "{{ worktree_path }}" | tr . -)
    tmux kill-session -t "=$S" 2>/dev/null || true
    """

    # Short aliases. Aliases run as shell commands (not as wt subcommand
    # names), so the `wt` prefix is explicit. `{{ args }}` forwards
    # positional args verbatim with shell-safe escaping.
    #
    # `--no-cd` on the two switch variants suppresses worktrunk's shell-cd
    # directive: the pre-start tmux hook already lands us in a session
    # whose cwd is the new worktree, so sourcing a `cd` in the *original*
    # shell would only drag the session we just left into the new dir —
    # surprising when we swap back to it later. Leaving the flag off the
    # plain `wt switch` / `wt switch --create` commands means the rare
    # out-of-tmux invocation still gets shell-follow behavior by default.
    [aliases]
    s = "wt switch --no-cd {{ args }}"
    c = "wt switch --create --no-cd {{ args }}"
    m = "wt merge {{ args }}"
    r = "wt remove {{ args }}"
    l = "wt list {{ args }}"

    # Sonnet over upstream's haiku default: commits are load-bearing
    # history and branch diffs can run thousands of lines, where
    # haiku's summarization drops important context. The rest of the
    # flags strip Claude Code's normal scaffolding (skills, CLAUDE.md,
    # tool loop, session write) so this behaves like a one-shot API
    # call rather than an interactive agent run.
    [commit.generation]
    command = "CLAUDECODE= MAX_THINKING_TOKENS=0 claude -p --no-session-persistence --model=sonnet --tools=''' --disable-slash-commands --setting-sources=''' --system-prompt='''"
  '';

  programs.bash = {
    enable = true;
    # Changes backup files `ls` color to dim cyan, otherwise they are invisible with solarized dark theme
    # Add Rust binaries to path, so `cargo install` works OOTB
    bashrcExtra = ''
      LS_COLORS=$(echo "$LS_COLORS" | sed 's/=00;90/=36;2/g')
      export PATH="$HOME/.cargo/bin:$PATH"
      # Read-only gh PAT, sops-decrypted at boot.
      [ -r /run/secrets/gh-token ] && export GH_TOKEN="$(cat /run/secrets/gh-token)"
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
    #settings = {
    #};
  };

  programs.direnv = {
    enable = true;
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
      set -g @continuum-save-interval '15'
    '';
  };

  programs.git = {
    enable = true;
    settings = {
      user = {
        name = "samuelburnham";
        email = "45365069+samuelburnham@users.noreply.github.com";
      };
      init.defaultBranch = "main";
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
