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
      # Disable Claude's `dangerouslyDisableSandbox: true` per-command
      # escape hatch. Claude's system prompt tells it to retry with that
      # flag whenever it sees sandbox-caused failures (EPERM, denied
      # paths, blocked hosts) — inside boxclaude those retries are
      # futile because the outer wrapper doesn't honor the flag. Setting
      # this to false makes the parameter a no-op so Claude stops trying.
      allowUnsandboxedCommands = false;
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

  boxclaudeSandbox = mkSandbox {
    name = "boxclaude";
    package = pkgs-master.claude-code;
    binPath = "bin/claude";
    # All sensitive capabilities (XDG runtime, SSH agent, DBus, wayland)
    # intentionally left at their default-off values — Claude has no use for
    # any of them, and the surface they expose (signing/push, clipboard
    # read, IPC) is exactly what we want kept away from a sandbox processing
    # untrusted text. Clipboard copy stays via terminal Ctrl-Shift-c outside
    # the sandbox.
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
