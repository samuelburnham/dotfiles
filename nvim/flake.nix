{
  description = "Neovim configuration with sandboxed launcher";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    # Tracks master for fast-moving leaf packages (e.g. claude-code) that
    # should update independently of the unstable channel
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    flake-parts.url = "github:hercules-ci/flake-parts";
    # Neovim flake
    nvf.url = "github:notashelf/nvf";
    # Declarative bwrap sandboxing. Pointed at a local fork so we can keep
    # a patch against launcher/main.go: upstream Setpgid's bwrap into its
    # own process group without handing it the tty, which leaves an
    # interactive app background w.r.t. the terminal when --new-session is
    # off (SIGTTIN on first read). The patch drops Setpgid so bwrap
    # inherits the launcher's pgroup — i.e., the shell's foreground pgroup
    # — so tty I/O and SIGWINCH both propagate.
    nixpak = {
      url = "github:samuelburnham/nixpak";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      nixpkgs-master,
      flake-parts,
      nvf,
      nixpak,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      # Systems we want to build for
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      perSystem =
        {
          system,
          pkgs,
          ...
        }:
        {
          packages =
            let
              pkgs-unstable = import nixpkgs-unstable { inherit system; };
              pkgs-master = import nixpkgs-master {
                inherit system;
                config.allowUnfree = true;
              };

              customNeovim =
                (nvf.lib.neovimConfiguration {
                  inherit pkgs;

                  extraSpecialArgs = {
                    inherit inputs pkgs-unstable;
                  };

                  modules = [
                    ./nvim.nix
                  ];
                }).neovim;

              mkNixPak = nixpak.lib.nixpak {
                inherit (pkgs) lib;
                inherit pkgs;
              };

              # Expose only nvim itself on sandbox PATH. Everything else (git,
              # coreutils, direnv, nix, tmux, claude-code, ...) comes from the
              # host profile dirs bound read-only below. Keeps the sandbox from
              # duplicating tools the host already has and avoids version drift.
              sandboxPath = pkgs.buildEnv {
                name = "boxvim-sandbox-path";
                paths = [ customNeovim ];
              };

              # Sandboxed nvim via nixpak. Closure-scoped /nix/store, filtered
              # session dbus, wayland socket, project dir bound rw at its host path,
              # ~/repos ro, credentials and sessions passed through.
              boxvimSandbox = mkNixPak {
                config =
                  { sloth, ... }:
                  {
                    app = {
                      package = customNeovim;
                      binPath = "bin/nvim";
                    };

                    dbus.policies = {
                      # Settings portal for auto-dark-mode.nvim's color-scheme query
                      "org.freedesktop.portal.Settings" = "talk";
                      "org.freedesktop.Notifications" = "talk";
                    };

                    bubblewrap = {
                      network = true;
                      # Full store binding. Closure-scoped was tempting but breaks
                      # any Nix activity producing new paths (flake fetches, builds,
                      # direnv activation) because the sandbox's /nix/store view is
                      # frozen at build time. Store contents are package files, not
                      # user home files — doesn't widen the threat model. Secrets
                      # stay out of the store via sops-nix anyway.
                      bindEntireStore = true;
                      extraStorePaths = [ sandboxPath ];

                      sockets.wayland = true;

                      bind.rw = [
                        # Host /tmp — shared rw. Scratch space for wl-copy,
                        # claude-code, and async plugin jobs. A sandbox-private
                        # /tmp was tried but nixpak applies `--tmpfs /tmp` AFTER
                        # `--bind`, so any nested bind gets clobbered.
                        "/tmp"
                        # Per-user runtime dir — home-manager's tmux puts its
                        # server socket under $XDG_RUNTIME_DIR/tmux-$UID/default,
                        # so smart-splits' cross-boundary `tmux set-option
                        # @pane-is-vim` (and any other tmux command from inside
                        # the sandbox) needs this reachable. systemd's
                        # pam_systemd guarantees XDG_RUNTIME_DIR on every login
                        # session, so sloth.env is safe.
                        (sloth.env "XDG_RUNTIME_DIR")
                        # All projects under ~/repos — live view of the
                        # host dir (new clones appear in the sandbox immediately
                        # without restarting). Keeping this rw for simplicity —
                        # sandbox can write to any sibling, accepted tradeoff.
                        (sloth.concat' sloth.homeDir "/repos")
                        # Project dir (current worktree) — resolved at runtime
                        (sloth.env "PROJECT_DIR")
                        # Main repo dir (shared .git + main working tree). Same as
                        # PROJECT_DIR for non-worktree repos — harmless re-bind.
                        # Gives the sandbox full git access: commit/add/fetch/push,
                        # checkout any branch, read files across all worktrees of
                        # this repo.
                        (sloth.env "MAIN_REPO_DIR")
                        # Credentials (Claude Code reads/writes session state)
                        (sloth.concat' sloth.homeDir "/.claude")
                        (sloth.concat' sloth.homeDir "/.claude.json")
                        # Shared session store — host path = inside path, portable
                        (sloth.concat' sloth.homeDir "/.local/share/nvf/sessions")
                        # SSH agent socket — host ssh-agent does the crypto, the
                        # sandbox reaches it via the same socket path. Private keys
                        # stay on host, never enter the sandbox.
                        (sloth.env "SSH_AUTH_SOCK")
                        # Cargo's registry index + downloaded crates live under
                        # ~/.cargo. Without this mount, rust-analyzer re-populates
                        # the index on every boxvim invocation.
                        (sloth.concat' sloth.homeDir "/.cargo")
                        # zkVM toolchain caches (zisk, sp1, risc0): each ships a
                        # rustup-style toolchain + program ELFs under its own
                        # ~/.<name> dir. Mounted rw so cargo-* subcommands and
                        # build scripts invoked from :terminal can read and
                        # populate them without re-downloading per session.
                        (sloth.concat' sloth.homeDir "/.zisk")
                        (sloth.concat' sloth.homeDir "/.sp1")
                        (sloth.concat' sloth.homeDir "/.risc0")
                        # Sandbox-private XDG dirs: host-side path under
                        # ~/.local/state/nixpak/boxvim/ maps to plain ~/.config etc.
                        # inside. Persistent (caches survive) but isolated from the
                        # host's real XDG dirs.
                        [
                          (sloth.mkdir (sloth.concat' sloth.homeDir "/.local/state/nixpak/boxvim/config"))
                          (sloth.concat' sloth.homeDir "/.config")
                        ]
                        [
                          (sloth.mkdir (sloth.concat' sloth.homeDir "/.local/state/nixpak/boxvim/local"))
                          (sloth.concat' sloth.homeDir "/.local")
                        ]
                        [
                          (sloth.mkdir (sloth.concat' sloth.homeDir "/.local/state/nixpak/boxvim/cache"))
                          (sloth.concat' sloth.homeDir "/.cache")
                        ]
                        # direnv allow-hashes live under ~/.local/share/direnv.
                        # Mounted AFTER the sandbox-private ~/.local above so this
                        # descendant overlays the parent instead of being clobbered
                        # by it — order matters: bwrap applies binds sequentially
                        # and a later bind at an ancestor path hides earlier binds
                        # at descendants.
                        (sloth.concat' sloth.homeDir "/.local/share/direnv")
                      ];

                      bind.ro = [
                        (sloth.concat' sloth.homeDir "/.config/nvf")
                        "/nix/var/nix/daemon-socket"
                        # Host nix client config — picks up experimental-features
                        # (nix-command, flakes) that NixOS sets in /etc/nix/nix.conf.
                        # Daemon already reachable via the socket above; this covers
                        # the client side so `nix build`, `nix flake update`, etc.
                        # work inside the sandbox.
                        "/etc/nix"
                        # Timezone data — Lean LSP and other programs fail with
                        # "no such file or directory /etc/localtime" without it
                        "/etc/localtime"
                        # User/group lookup for UID→username resolution (openssh
                        # needs this to resolve UID 1000 → sam; other nss-using
                        # tools read it too)
                        "/etc/passwd"
                        "/etc/group"
                        # Host ~/.bashrc — the user-level rc (aliases + direnv /
                        # starship / zoxide hooks). Mounted directly so there's
                        # one source of truth, no synthetic copy to drift.
                        (sloth.concat' sloth.homeDir "/.bashrc")
                        # System bashrc + profile. NixOS's bash-interactive is
                        # patched with SYS_BASHRC=/etc/bashrc; for interactive
                        # non-login shells (what :terminal spawns) bash sources
                        # this BEFORE ~/.bashrc, and it's where bash-completion
                        # gets loaded (via /etc/static/bashrc → bash_completion.sh).
                        # Both are symlinks into /etc/static (already bound), so
                        # binding the symlink files themselves is enough — without
                        # them tab-completion silently doesn't work in :terminal.
                        "/etc/bashrc"
                        "/etc/profile"
                        # Git config (home-manager's programs.git writes to
                        # ~/.config/git/config on NixOS). Overlays on the
                        # sandbox-private ~/.config map since bind.ro is layered
                        # after bind.rw. ~/.gitconfig fallback for hand-written cfg.
                        (sloth.concat' sloth.homeDir "/.config/git")
                        (sloth.concat' sloth.homeDir "/.gitconfig")
                        # Direnv config (home-manager's programs.direnv.nix-direnv
                        # writes a direnvrc that sources nix-direnv by absolute
                        # store path). Without this the sandbox's direnv falls back
                        # to its built-in `use flake`, which calls `mkdir` and `nix`
                        # by short name and fails once the devShell's PATH drops
                        # them.
                        (sloth.concat' sloth.homeDir "/.config/direnv")
                        # worktrunk config — the `wt` binary reads aliases
                        # (`wt s`/`wt c`/`wt m`/...) and pre-start/pre-remove hooks
                        # from this file. Without the mount, :terminal shells see
                        # a bare `wt` that rejects the aliases and runs no hooks.
                        (sloth.concat' sloth.homeDir "/.config/worktrunk")
                        # Dotfiles overlay — src/dst resolved by the outer wrapper:
                        # - active dotfiles project → /dev/null → /dev/null (no-op)
                        # - anything else → ~/repos/dotfiles → ~/repos/dotfiles (ro)
                        [
                          (sloth.env "DOTFILES_RO_SRC")
                          (sloth.env "DOTFILES_RO_DST")
                        ]
                        # known_hosts so ssh/git-over-ssh doesn't prompt on first
                        # connection to github.com etc. Private keys are NOT mounted
                        # — agent-forwarding above handles auth.
                        (sloth.concat' sloth.homeDir "/.ssh/known_hosts")
                        # Host profile dirs. nix-direnv's `use flake` runs
                        # `nix print-dev-env` non-pure, which bakes the caller's
                        # PATH into the cached profile_rc. That cache ends up
                        # referencing these dirs, so they must resolve inside the
                        # sandbox for `git`, `which`, etc. to work post-reload.
                        # Each is a symlink tree into /nix/store (already bound
                        # via bindEntireStore), so this is essentially free.
                        "/etc/profiles"
                        # /etc/profiles/per-user/<user> is a symlink to
                        # /etc/static/profiles/per-user/<user> on NixOS, so the
                        # static tree must be bound too or the symlink dangles
                        # and every tool in the user profile looks missing.
                        "/etc/static"
                        "/run/current-system/sw"
                        "/run/wrappers"
                        "/nix/var/nix/profiles"
                      ];

                      env = {
                        # Prepend sandboxPath to the inherited PATH so bundled
                        # tools win, but direnv-loaded project paths (set by host
                        # shell before `nvim` was invoked) still carry in. This
                        # mirrors running nvim unsandboxed: LSPs see their
                        # binaries on startup.
                        PATH = sloth.concat [
                          "${sandboxPath}/bin:"
                          (sloth.env "PATH")
                        ];
                        # Host's $SHELL points at /run/current-system/... which isn't
                        # mounted. Override to a sandbox-valid path.
                        SHELL = "${pkgs.bash}/bin/bash";
                        BOXVIM = "1";
                        NIX_REMOTE = "daemon";
                        LANG = "C.UTF-8";
                        LC_ALL = "C.UTF-8";
                        # CA bundle for TLS verification (claude-code, curl, nix fetches)
                        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
                        NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
                        NODE_EXTRA_CA_CERTS = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
                        # Dir of hashed CAs — OpenSSL's compile-time fallback points
                        # at a non-existent store path if this is unset
                        SSL_CERT_DIR = "${pkgs.cacert}/etc/ssl/certs";
                        # ssh-agent socket path — paired with the bind.rw mount above
                        SSH_AUTH_SOCK = sloth.env "SSH_AUTH_SOCK";
                        # Tmux session identity — paired with the /tmp bind above.
                        # Empty string default so nvim launched outside tmux doesn't
                        # panic (sloth.env "TMUX" would abort on missing).
                        TMUX = sloth.envOr "TMUX" "";
                      };

                      # newSession would pass --new-session to bwrap, calling
                      # setsid() and detaching nvim from the controlling terminal.
                      # Historically this mitigated CVE-2017-5226 (TIOCSTI ioctl
                      # input injection), but it also prevents tmux's pty-resize
                      # SIGWINCH from reaching the sandboxed nvim — nvim keeps
                      # rendering at its pre-split width and paints over new tmux
                      # panes. On Linux 6.2+ the kernel gates TIOCSTI behind
                      # `dev.tty.legacy_tiocsti` (0 by default here, verified at
                      # /proc/sys/dev/tty/legacy_tiocsti); combined with bwrap
                      # dropping all caps + PR_SET_NO_NEW_PRIVS, the attack is
                      # blocked one layer down. Leave newSession off so SIGWINCH
                      # lands. Re-enable if ever running on a kernel without
                      # CONFIG_LEGACY_TIOCSTI=n / the sysctl defaulted to 0.
                      newSession = false;
                      dieWithParent = true;
                    };
                  };
              };

              # Outer wrapper: resolves
              #   $PROJECT_DIR   — git worktree root (cwd if outside a repo)
              #   $MAIN_REPO_DIR — main repo root (== PROJECT_DIR for non-worktree
              #                    repos; the shared-.git parent for worktrees).
              #                    Mounted rw so all git ops work and you can read
              #                    files across all worktrees of the same repo.
              nvim = pkgs.writeShellScriptBin "nvim" ''
                set -euo pipefail
                WORK_DIR="''${PWD}"
                export PROJECT_DIR="$(${pkgs.git}/bin/git -C "$WORK_DIR" rev-parse --show-toplevel 2>/dev/null || echo "$WORK_DIR")"
                export MAIN_REPO_DIR="$PROJECT_DIR"
                GIT_COMMON="$(${pkgs.git}/bin/git -C "$WORK_DIR" rev-parse --git-common-dir 2>/dev/null || true)"
                if [ -n "$GIT_COMMON" ]; then
                  GIT_COMMON_ABS="$(cd "$WORK_DIR" && ${pkgs.coreutils}/bin/realpath -e "$GIT_COMMON" 2>/dev/null || true)"
                  if [ -n "$GIT_COMMON_ABS" ] && [ -d "$GIT_COMMON_ABS" ]; then
                    export MAIN_REPO_DIR="$(${pkgs.coreutils}/bin/dirname "$GIT_COMMON_ABS")"
                  fi
                fi
                cd "$WORK_DIR"

                # Pin nvim's RPC socket to a unique-per-launch path so the
                # auto-server never relies on the sandbox PID. bwrap's
                # --unshare-pid puts every boxvim instance at a low PID
                # (~3) inside its own pidns, but $XDG_RUNTIME_DIR is
                # shared with the host — so nvim's default
                # nvf.<pid>.<n> socket name collides with stale sockets
                # left by prior sessions. After enough collisions the
                # auto-server gives up, v:servername comes up empty,
                # :terminal can't export $NVIM, and flatten.nvim opens
                # nested nvim on every git commit instead of routing the
                # buffer back to the host. $$ here is the wrapper's host
                # PID, which is unique across concurrent launches.
                export NVIM_LISTEN_ADDRESS="''${XDG_RUNTIME_DIR:-/run/user/$UID}/nvim-boxvim-$$.sock"

                # Conditional ro overlay on ~/repos/dotfiles:
                # - If dotfiles IS the active project (either directly or via a
                #   dotfiles worktree, so MAIN_REPO_DIR = ~/repos/dotfiles), or
                #   the path doesn't exist yet, bind /dev/null → /dev/null as a
                #   no-op so PROJECT_DIR's rw bind stays in effect.
                # - Otherwise overlay ~/repos/dotfiles as ro so cross-project
                #   nvim sessions can read it (reference configs, copy snippets)
                #   but can't accidentally clobber it — Claude auto-accept in an
                #   unrelated project can still touch anything else under ~/repos.
                DOTFILES="$HOME/repos/dotfiles"
                if [ ! -d "$DOTFILES" ] || [ "$PROJECT_DIR" = "$DOTFILES" ] || [ "$MAIN_REPO_DIR" = "$DOTFILES" ]; then
                  export DOTFILES_RO_SRC=/dev/null
                  export DOTFILES_RO_DST=/dev/null
                else
                  export DOTFILES_RO_SRC="$DOTFILES"
                  export DOTFILES_RO_DST="$DOTFILES"
                fi

                # BOXVIM_DEBUG=1 wraps nvim in strace, logging every file lookup.
                # After repro, find missing bind mounts with:
                #   grep -E 'ENOENT|EACCES|EROFS' /tmp/boxvim-debug.log | sort -u
                if [ "''${BOXVIM_DEBUG:-}" = "1" ]; then
                  exec ${pkgs.strace}/bin/strace -f -e trace=file \
                    -o /tmp/boxvim-debug.log \
                    ${boxvimSandbox.config.script}/bin/nvim "$@"
                fi
                exec ${boxvimSandbox.config.script}/bin/nvim "$@"
              '';
            in
            {
              # Sandboxed nvim — the default for normal use
              default = nvim;
              nvim = nvim;
              # Raw nvf-built nvim, no sandbox. Escape hatch for debugging plugin
              # or LSP issues where you need to rule out the sandbox as a cause.
              nvim-unwrapped = customNeovim;
            };

          # `nix fmt` entry point. nixfmt-tree = treefmt pre-configured with
          # nixfmt (RFC 166), respects .gitignore, caches per-file, parallel.
          formatter = pkgs.nixfmt-tree;
        };
    };
}
