# Home-manager entry point for the dev microvm. The VM runs the
# interactive dev environment — tmux, nvim, claude, shell, builds — which
# you reach by ssh from the host's ghostty. No compositor or terminal-
# emulator config here (those live on the host); the VM only runs a shell.
{
  pkgs,
  lib,
  inputs,
  ...
}:
{
  imports = [
    ../modules/base.nix
    ../modules/alias.nix
    ../modules/claude.nix
    ../modules/worktrunk.nix
  ];

  home.packages = [
    inputs.self.packages.${pkgs.system}.nvim
  ];
  # The VM is a brand-new system with no prior HM state; override
  # base.nix's host-side 25.05 pin so it doesn't collide with the
  # microvm host module's 25.11 default.
  home.stateVersion = lib.mkForce "25.11";

  # No SSH agent in the VM. Private GitHub repos still clone with their
  # SSH-style URLs because git transparently rewrites git@github.com /
  # ssh://git@github.com fetches to HTTPS, which authenticate with the
  # forwarded read-only GH_TOKEN (gh credential helper, base.nix). You run
  # `git clone git@github.com:org/repo` unchanged; the rewrite is internal.
  # VM-only — the host keeps real SSH, so push (write, hardware key) works
  # there. Pushing from the VM is intentionally unsupported (read-only
  # token → 403); push from the host.
  programs.git.settings.url."https://github.com/".insteadOf = [
    "git@github.com:"
    "ssh://git@github.com/"
  ];

  # The dev microvm is the isolated sandbox this tooling is meant to run in,
  # so default to "auto" mode: Claude auto-approves actions it classifies as
  # safe and still blocks risky ones / suspected prompt injection. This plain
  # value overrides base.nix's prompting default; the workstations keep it.
  programs.claude-code.settings.permissions.defaultMode = "auto";

  # Make cargo fetch git deps via the git CLI so private deps go through
  # the credential helper + token (and the rewrite above) rather than
  # cargo's built-in fetcher, which ignores them.
  home.sessionVariables.CARGO_NET_GIT_FETCH_WITH_CLI = "true";
}
