# Sandboxed claude-code wrapper. Imported from apps/flake.nix; returns the
# `pkgs.writeShellScriptBin` derivation that the flake exposes as
# `packages.<system>.claude`.
#
# Owns claude's settings.json content too — rendered to /nix/store on each
# `nix run apps#claude` and bind-mounted over ~/.claude/settings.json inside
# the sandbox. Edits to this file apply on the next launch without
# nixos-rebuild. Host (unsandboxed) claude won't see these settings, but the
# only thing invoking host claude is automation that already passes
# `--setting-sources=''` to bypass settings.json entirely.
{
  pkgs,
  pkgs-master,
  mkSandbox,
  sandboxEnvSetup,
}:
let
  rustfmtHook = ''
    f=$(jq -r '.tool_input.file_path')
    if [ -z "$f" ] || [ "$f" = "null" ]; then exit 0; fi
    d=$(dirname "$f")
    while [ "$d" != / ] && [ ! -f "$d/.envrc" ]; do d=$(dirname "$d"); done
    if [ -f "$d/.envrc" ]; then
      direnv exec "$d" rustfmt "$f"
    else
      rustfmt "$f"
    fi
  '';

  settings = {
    theme = "dark";
    sandbox = {
      enabled = true;
      # Auto-approve any bash command the inner sandbox can run; ones
      # it can't sandbox (writes outside cwd/Edit rules, off-allowlist
      # hosts, excludedCommands) still fall back to the permission
      # flow. The static analyzer keeps classifying — this just
      # silences the no-op prompts for sandbox-safe commands (the
      # compound `cd && ...` warning is the most common one).
      autoAllowBashIfSandboxed = true;
      # Allow Claude's `dangerouslyDisableSandbox: true` per-command
      # escape hatch so commands can opt out of the inner sandbox when
      # asked to. The outer nixpak wrapper still confines the process,
      # so "unsandboxed" here only means bypassing Claude's own static
      # analyzer — filesystem and network limits still apply.
      allowUnsandboxedCommands = true;
      # Network hosts the inner sandbox is allowed to reach. Specifying
      # this replaces Claude Code's built-in defaults (api.github.com,
      # github.com), so re-enumerate them alongside any additions.
      # crates.io's registry index for cargo fetch/update without prompts.
      network.allowedDomains = [
        "github.com"
        "api.github.com"
        "index.crates.io"
      ];
    };
    permissions = {
      disableBypassPermissionsMode = "disable";
      # Read/Glob/Grep over ~/repos lets Claude reach into sibling repos
      # (clones, forks, other projects) from any session without prompting.
      # No write permissions granted at that scope.
      #
      # Build/test/inspect commands are allowed without prompting — they're
      # sandbox-safe, idempotent, and the prompt-loop for "yes, run cargo
      # check again" is pure friction. `:*` matches any argument suffix.
      allow = [
        "Read(/home/sam/repos/**)"
        "Glob(/home/sam/repos/**)"
        "Grep(/home/sam/repos/**)"
        # Extends bash's filesystem write scope under Claude's inner
        # sandbox. Without this, the static analyzer flags writes
        # outside the current working directory and falls back to a
        # prompt, even though boxclaude already allows the write at the
        # OS level (~/repos is bind-mounted rw).
        "Edit(/home/sam/repos/**)"
        "Read(/home/sam/.cargo/**)"
        "Glob(/home/sam/.cargo/**)"
        "Grep(/home/sam/.cargo/**)"
        # cargo writes to ~/.cargo/registry, ~/.cargo/bin, etc. on
        # install/update; the rw bind from mkSandbox allows it at the
        # OS level, this allows it at the static-analyzer level too.
        "Edit(/home/sam/.cargo/**)"
        # /nix/store is read-only at the kernel level and content-addressed.
        # Sensitive material lives in /run/secrets via sops-nix, not the
        # store, so granting read here only exposes rendered configs (system
        # topology, service settings) — recon info, not credentials.
        "Read(/nix/store/**)"
        "Glob(/nix/store/**)"
        "Grep(/nix/store/**)"
        "Bash(cargo build:*)"
        "Bash(cargo check:*)"
        "Bash(cargo run:*)"
        "Bash(cargo test:*)"
        "Bash(cargo fmt:*)"
        "Bash(cargo clippy:*)"
        "Bash(cargo xclippy:*)"
        "Bash(lake build:*)"
        "Bash(lake exe:*)"
        "Bash(lake test:*)"
        "Bash(nix develop:*)"
        "Bash(nix build:*)"
        "Bash(nix fmt:*)"
        "Bash(nix flake show:*)"
        "Bash(nix flake metadata:*)"
        "Bash(nix eval:*)"
        "Bash(grep:*)"
        "Bash(rg:*)"
        "Bash(fd:*)"
        "Bash(jq:*)"
        "Bash(tail:*)"
        "Bash(head:*)"
        "Bash(wc:*)"
        "Bash(git status:*)"
        "Bash(git log:*)"
        "Bash(git diff:*)"
        "Bash(git show:*)"
        "Bash(git branch:*)"
        "Bash(git remote -v:*)"
        # The bound GH_TOKEN is a read-only fine-grained PAT, so even
        # accidentally-issued POST/PATCH/DELETE calls fail at GitHub's side.
        "Bash(gh api:*)"
        "Bash(git check-ignore:*)"
        "Bash(cargo bench:*)"
        # Common shell utilities the static analyzer still prompts for
        # despite autoAllowBashIfSandboxed — explicit allow is cheap.
        "Bash(ls:*)"
        "Bash(find:*)"
        "Bash(xxd:*)"
        "Bash(awk:*)"
        # Public hosts already in sandbox.network.allowedDomains above —
        # mirror them here so WebFetch (the model-facing tool) doesn't
        # prompt either.
        "WebFetch(domain:github.com)"
        "WebFetch(domain:api.github.com)"
        "WebFetch(domain:index.crates.io)"
        # /tmp is sandbox-scoped (XDG_RUNTIME_DIR remap, ephemeral binds);
        # reads here are safe and recur across cargo/lake/zisk workflows.
        "Read(/tmp/**)"
        "Glob(/tmp/**)"
        "Grep(/tmp/**)"
      ];
    };
    # Auto-format Rust files after Claude edits them. The walk-up to
    # .envrc resolves the *file's* project rather than Claude's CWD, so
    # editing a Rust file in project B from a Claude session launched
    # in project A still uses B's pinned toolchain via direnv. Plain
    # `rustfmt` falls through if no .envrc is found.
    hooks = {
      PostToolUse = [
        {
          matcher = "Edit|Write|MultiEdit";
          hooks = [
            { type = "command"; "if" = "Edit(**/*.rs)"; command = rustfmtHook; }
            { type = "command"; "if" = "Write(**/*.rs)"; command = rustfmtHook; }
            { type = "command"; "if" = "MultiEdit(**/*.rs)"; command = rustfmtHook; }
          ];
        }
      ];
    };
  };

  settingsFile = pkgs.writeText "claude-settings.json" (builtins.toJSON settings);

  # Smart wl-paste that resolves file:// URIs to image/png when the clipboard
  # holds only file-reference MIME types. GTK4 apps (Loupe, Nautilus) and
  # hyprshot on Hyprland put text/uri-list instead of raw image bytes; Claude
  # Code's checkImage/saveImage only speak image/* via `wl-paste -l` and
  # `wl-paste --type image/png`. The wrapper synthesises image/png in -l
  # output when the first file:// URI points to a local file, and falls back
  # to magick conversion on image/png requests when no direct type exists.
  # magick is reachable via /nix/store (bindEntireStore = true).
  wlPasteScript = pkgs.writeShellScript "wl-paste" ''
    real=${pkgs.wl-clipboard}/bin/wl-paste

    # Decoded path of the clipboard's first file:// URI, or empty if none.
    clip_file() {
      local uri enc path
      uri=$("$real" --type text/uri-list 2>/dev/null | head -1 | tr -d '\r\n')
      [ "''${uri#file://}" != "$uri" ] || return 1
      enc=''${uri#file://}
      path=$(printf '%b' "''${enc//%/\\x}")
      [ -f "$path" ] && printf '%s' "$path"
    }

    for arg in "$@"; do case "$arg" in
      -l|--list-types)
        types=$("$real" -l 2>/dev/null)
        printf '%s\n' "$types"
        printf '%s' "$types" | grep -q '^image/' && exit 0
        [ -n "$(clip_file)" ] && printf 'image/png\n'
        exit 0
        ;;
      image/png)
        "$real" "$@" 2>/dev/null && exit 0
        path=$(clip_file) && exec ${pkgs.imagemagick}/bin/magick "$path" png:-
        exit 1
        ;;
    esac; done
    exec "$real" "$@"
  '';

  # Package providing wl-paste (the smart wrapper above) and wl-copy
  # (pass-through to wl-clipboard) at bin/ without a collision from
  # wl-clipboard's own wl-paste when added to extraPaths.
  wlPasteWrapper = pkgs.runCommand "wl-paste-wrapper" { } ''
    mkdir -p $out/bin
    cp ${wlPasteScript} $out/bin/wl-paste
    chmod +x $out/bin/wl-paste
    ln -s ${pkgs.wl-clipboard}/bin/wl-copy $out/bin/wl-copy
  '';

  boxclaudeSandbox = mkSandbox {
    name = "boxclaude";
    package = pkgs-master.claude-code;
    binPath = "bin/claude";
    # wlPasteWrapper lands at sandboxPath/bin, which nixpak prepends to PATH.
    # Provides wl-paste (URI-resolving wrapper) and wl-copy so Claude Code's
    # image-paste subprocesses find them — ~/.nix-profile is not mounted.
    # python3 is exposed so Claude can run quick one-off scripts (URL
    # decoding, byte-level file edits, etc.) without grovelling through
    # /nix/store for an absolute path.
    extraPaths = [
      wlPasteWrapper
      pkgs.python3
    ];
    # XDG runtime, SSH agent, and DBus remain off — Claude has no need for
    # signing/push or IPC and the exposure isn't worth it.
    # Wayland socket is exposed below for image-paste clipboard access.
    extraBindRw = sloth: [
      # All projects under ~/repos — live view of the host dir so cross-repo
      # Read/Glob/Grep/Edit (declared in settings.allow above) reaches sibling
      # checkouts without prompting. Boxvim deliberately omits this; Claude
      # needs it for the documented cross-repo workflow.
      (sloth.concat' sloth.homeDir "/repos")
      # Claude session state (OAuth tokens, project history, history.jsonl,
      # paste cache, etc.). Boxvim doesn't read or write these.
      (sloth.concat' sloth.homeDir "/.claude")
      (sloth.concat' sloth.homeDir "/.claude.json")
    ];
    extraBindRo = sloth: [
      # Wayland compositor socket for image-paste clipboard reads. XDG_RUNTIME_DIR
      # is overridden to /tmp inside the sandbox, so nixpak's sockets.wayland
      # (which binds the socket at the host path) lands at the wrong location.
      # Bind it explicitly at /tmp/$WAYLAND_DISPLAY — where apps resolve
      # $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY — so wl-paste and Claude Code's
      # clipboard reader can find it.
      [
        (sloth.concat [ sloth.runtimeDir "/" (sloth.envOr "WAYLAND_DISPLAY" "wayland-0") ])
        (sloth.concat [ "/tmp/" (sloth.envOr "WAYLAND_DISPLAY" "wayland-0") ])
      ]
      # ~/Pictures so wlPasteScript (and Claude's own file-URL resolution) can
      # read screenshot files referenced via clipboard text/uri-list. hyprshot
      # writes here by default even though XDG_SCREENSHOTS_DIR points at the
      # Screenshots subdir, so mount the parent to cover both.
      (sloth.concat' sloth.homeDir "/Pictures")
      # Overlay the apps-generated settings.json on top of the bind-mounted
      # ~/.claude — this is the bit that lets edits to this file apply
      # without a nixos-rebuild. Order matters: bind.ro is applied after
      # bind.rw, so this descendant cleanly overlays the parent dir bind.
      [
        "${settingsFile}"
        (sloth.concat' sloth.homeDir "/.claude/settings.json")
      ]
      # Read access to user config (pop-shell JSON, gh prefs, etc.). No
      # credentials currently live here (gh token comes from GH_TOKEN env);
      # audit before installing apps that stash secrets under ~/.config.
      (sloth.concat' sloth.homeDir "/.config")
      # Shadow cargo's crates.io tokens with /dev/null. ~/.cargo is bound
      # rw by mkSandbox; these later ro-binds override the relevant files
      # so reads return empty and cargo treats the sandbox as logged out.
      # Run `cargo publish` on the host if you need real credentials.
      [
        "/dev/null"
        (sloth.concat' sloth.homeDir "/.cargo/credentials.toml")
      ]
      [
        "/dev/null"
        (sloth.concat' sloth.homeDir "/.cargo/credentials.toml.bak")
      ]
      [
        "/dev/null"
        (sloth.concat' sloth.homeDir "/.cargo/credentials")
      ]
    ];
    extraEnv = {
      # Claude's color renderer downgrades to ANSI named colors (which
      # render through the terminal palette — muted under Solarized)
      # when it detects $TMUX. boxclaude doesn't drive tmux, so clear
      # the var to keep truecolor RGB rendering inside tmux panes.
      TMUX = "";
      # Pin the Claude version to whatever pkgs-master ships at build
      # time. Without this Claude phones home and self-updates the node
      # bundle on disk, drifting from the Nix-managed copy and breaking
      # reproducibility (and re-running on each new shell if the
      # writable layer doesn't persist). See claudebox.
      DISABLE_AUTOUPDATER = "1";
    };
  };
in
pkgs.writeShellScriptBin "claude" ''
  ${sandboxEnvSetup}
  exec ${boxclaudeSandbox.config.script}/bin/claude "$@"
''
