# Claude Code — the self-pinned release plus its settings.json, CLAUDE.md,
# sandbox, permissions, and hooks. Kept OUT of base.nix and imported only
# where it's wanted, so it isn't in every closure by default:
#   - the disposable/isolated environments — the dev microvm (dev-vm.nix)
#     and the Ubuntu cloud box (ubuntu.nix) — which default to "auto" mode;
#   - the bare-metal workstations (desktop.nix, laptop.nix), where claude
#     runs directly against real repos — and, on the desktop, debugs the
#     host-side Hyprland/waybar setup the dev VM can't see.
# The workstations are the hosts with the real ~/.config, /run/secrets, and
# host services in reach, so they deliberately keep the prompting
# `defaultMode` and the bubblewrap `sandbox` below — claude can't act
# unattended there.
{
  inputs,
  pkgs,
  pkgs-master,
  lib,
  config,
  ...
}:
let
  rustfmtHook = ''
    f=$(jq -r '.tool_input.file_path')
    if [ -z "$f" ] || [ "$f" = "null" ]; then exit 0; fi
    d=$(dirname "$f")
    while [ "$d" != / ] && [ ! -f "$d/.envrc" ]; do d=$(dirname "$d"); done
    # direnv runs rustfmt from the repo's devshell (pinned toolchain). It is
    # intentionally absent on the microvm host (see desktop.nix), so fall back
    # to a plain rustfmt when direnv isn't on PATH.
    if [ -f "$d/.envrc" ] && command -v direnv >/dev/null 2>&1; then
      direnv exec "$d" rustfmt "$f"
    else
      rustfmt "$f"
    fi
  '';
in
{
  # settings.json + CLAUDE.md. home-manager writes settings.json to
  # ~/.claude/settings.json in each environment that imports this module; that
  # ~/.claude is the environment's own state (the dev VM's lives on its private
  # home volume), independent of every other.
  programs.claude-code = {
    enable = true;
    # nixpkgs-master only provides the build recipe (autopatchelf + the wrapper
    # that wires in ripgrep/bubblewrap/socat); the actual release is self-pinned
    # here so it tracks upstream independently of channel lag.
    # Bump: set `version` to https://downloads.claude.ai/claude-code-releases/latest
    # and paste `.platforms."linux-x64".checksum` from that release's manifest.json.
    #
    # Routed through `package` rather than home.packages because `lspServers`
    # below only takes effect when the module can wrap the binary: it builds a
    # symlinkJoin launcher that injects `--plugin-dir` (carrying the generated
    # .lsp.json) and refuses a null package. The module adds that wrapper to
    # home.packages itself, so it must not also be listed there by hand.
    package = pkgs-master.claude-code.overrideAttrs (old: rec {
      version = "2.1.190";
      src = pkgs.fetchurl {
        url = "https://downloads.claude.ai/claude-code-releases/${version}/linux-x64/claude";
        sha256 = "0684e28517cc785ab8d19feb5dad3381eab4abc97bf6fce07bc534dc88040b27";
      };
    });
    settings = {
      theme = "dark";
      # Opt out of non-essential network calls individually rather than via the
      # CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC umbrella — and crucially NOT via
      # DISABLE_TELEMETRY. Both put claude into "restricted traffic" mode (the
      # internal Yns() returns non-"default"), which short-circuits GrowthBook
      # feature-flag evaluation. The `allow_remote_control` gate then never
      # resolves true, so the /remote-control command is never registered (the
      # same path also gates 1M context and Agent View). Telemetry and flag
      # delivery share one upstream code path, so there is no config-only way to
      # suppress telemetry while keeping flags live — keeping it off means losing
      # Remote Control. We therefore disable only the things on independent
      # paths: error reporting and the feedback command. The autoupdater stays
      # off because claude is nix-pinned and must not self-update out from under
      # home-manager (the nixpkgs wrapper already sets this too).
      env = {
        DISABLE_ERROR_REPORTING = "1";
        DISABLE_AUTOUPDATER = "1";
        DISABLE_FEEDBACK_COMMAND = "1";
        # The periodic "was this helpful?" popup is a separate survey from the
        # /feedback command above. Its show-check honours this var before the
        # allow_product_feedback flag, so it's a clean off switch — no bearing
        # on the flag/Remote-Control tradeoff described above.
        CLAUDE_CODE_DISABLE_FEEDBACK_SURVEY = "1";
      };
      sandbox = {
        enabled = true;
        autoAllowBashIfSandboxed = true;
        allowUnsandboxedCommands = true;
        # cargo's caches all live under ~/.cargo: the registry index/src, git
        # checkouts (git/db, git/checkouts), and the .package-cache lock.
        # Without an explicit allowWrite the sandbox keeps the real ~/.cargo
        # read-only and redirects home writes to a shadow tree, so cargo can't
        # populate or update its cache — fetching a git dependency fails with
        # EROFS on ~/.cargo/git/db. Listing it here writes through to the real
        # cache, which also keeps it persistent and shared across runs instead
        # of rebuilt into a throwaway shadow each time.
        filesystem.allowWrite = [ "${config.home.homeDirectory}/.cargo" ];
        network.allowedDomains = [
          "github.com"
          "api.github.com"
          "index.crates.io"
        ];
      };
      permissions = {
        # Prompt before applying Edit/Write/MultiEdit by default. mkDefault so
        # the disposable/isolated VMs (dev microvm, Ubuntu cloud box) default
        # to "auto" mode with a plain value (see dev-vm.nix / ubuntu.nix). The
        # persistent workstations keep this prompting default but leave the
        # unattended modes unlocked, so auto/bypass can still be toggled on by
        # hand (Shift+Tab) for a session. Bash and other tools still pass the
        # allow-list and inner sandbox regardless of mode.
        defaultMode = lib.mkDefault "default";
        allow = [
          "Read(${config.home.homeDirectory}/repos/**)"
          "Glob(${config.home.homeDirectory}/repos/**)"
          "Grep(${config.home.homeDirectory}/repos/**)"
          "Edit(${config.home.homeDirectory}/repos/**)"
          "Read(${config.home.homeDirectory}/.cargo/**)"
          "Glob(${config.home.homeDirectory}/.cargo/**)"
          "Grep(${config.home.homeDirectory}/.cargo/**)"
          "Edit(${config.home.homeDirectory}/.cargo/**)"
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
          "Bash(gh api:*)"
          "Bash(git check-ignore:*)"
          "Bash(cargo bench:*)"
          "Bash(ls:*)"
          "Bash(find:*)"
          "Bash(xxd:*)"
          "Bash(awk:*)"
          "WebFetch(domain:github.com)"
          "WebFetch(domain:api.github.com)"
          "WebFetch(domain:index.crates.io)"
          "Read(/tmp/**)"
          "Glob(/tmp/**)"
          "Grep(/tmp/**)"
        ];
        # Human-only actions (destroying/applying infra, driving cloud CLIs).
        # The authoritative, tamper-proof copy is the root-owned
        # managed-settings.json (common/claude-managed-settings.nix); this
        # user-level copy is a best-effort fallback for hosts that don't
        # deploy the managed file (the standalone Ubuntu box) and shares the
        # same source list so the two can't drift.
        deny = import ../../common/claude-deny-list.nix;
      };
      hooks = {
        # tmux-assistant-resurrect session tracking. SessionStart writes a state
        # file keyed by this claude process's PID (session id + cwd + model),
        # which tmux-resurrect's post-save hook reads to record what each pane
        # was running; SessionEnd removes it. Keying by PID is what lets two
        # conversations in the same directory be resumed into their own panes.
        # See base.nix for the matching resurrect save/restore hooks.
        SessionStart = [
          {
            matcher = "";
            hooks = [
              {
                type = "command";
                command = "bash '${inputs.tmux-assistant-resurrect}/hooks/claude-session-track.sh'";
              }
            ];
          }
        ];
        SessionEnd = [
          {
            matcher = "";
            hooks = [
              {
                type = "command";
                command = "bash '${inputs.tmux-assistant-resurrect}/hooks/claude-session-cleanup.sh'";
              }
            ];
          }
        ];
        PostToolUse = [
          {
            matcher = "Edit|Write|MultiEdit";
            hooks = [
              {
                type = "command";
                "if" = "Edit(**/*.rs)";
                command = rustfmtHook;
              }
              {
                type = "command";
                "if" = "Write(**/*.rs)";
                command = rustfmtHook;
              }
              {
                type = "command";
                "if" = "MultiEdit(**/*.rs)";
                command = rustfmtHook;
              }
            ];
          }
        ];
      };
    };
    # rust-analyzer for `.rs` files, giving claude LSP diagnostics and code
    # intelligence. The module serializes this to a .lsp.json inside the
    # `--plugin-dir` it wraps the binary with (see `package` above) — there is
    # no settings.json key for it, so it lives here rather than under `settings`.
    #
    # `command` is a bare name resolved from PATH at spawn — NOT via `direnv
    # exec` like the rustfmt hook above — so the server only starts when claude
    # is launched from a direnv-activated devshell that provides rust-analyzer.
    # We deliberately don't install it globally; on hosts/repos without it on
    # PATH the spawn just fails and is skipped, rather than erroring the session.
    lspServers = {
      rust-analyzer = {
        command = "rust-analyzer";
        extensionToLanguage = {
          ".rs" = "rust";
        };
      };
    };
    context = ''
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

      # Git pushing

      Never run `git push`, or otherwise push to a remote. Commit locally when
      asked, but pushing is always the user's action: when a push is the next
      step, give the user the exact command to run instead of running it.

      # Commit attribution

      Never add a `Co-Authored-By: Claude ...` trailer — or any Claude/Anthropic
      co-author or attribution line — to git commits or PR descriptions.
    '';
  };

  # Claude Code treats ~/.claude/settings.json as its own mutable runtime
  # config and periodically rewrites it — replacing the read-only store
  # symlink installed above with a plain file (observed reset to `{}`).
  # Without force, the next activation sees a non-symlink in the way and
  # tries to back it up; with backupFileExtension set that backup collides
  # with a prior switch's leftover .bak and aborts the whole home-manager
  # activation. force makes home-manager overwrite the stray file in place,
  # re-asserting the managed settings on every switch with no backup step,
  # so a runtime rewrite can never wedge a rebuild.
  # Keyed to the module's own configDir (absolute, ~/.claude by default) so
  # this merges into the module's home.file entry rather than colliding with
  # it as a second entry pointing at the same target.
  home.file."${config.programs.claude-code.configDir}/settings.json".force = true;
}
