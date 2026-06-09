# Home-manager entry point for the dev microvm. The VM runs the
# interactive dev environment — tmux, nvim, claude, shell, builds — which
# you reach by ssh from the host's ghostty. No compositor or terminal-
# emulator config here (those live on the host); the VM only runs a shell.
{
  pkgs,
  lib,
  inputs,
  username,
  ...
}:
{
  imports = [
    ./base.nix
    ./alias.nix
  ];

  home.username = username;
  home.homeDirectory = "/home/${username}";

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

  # Make cargo fetch git deps via the git CLI so private deps go through
  # the credential helper + token (and the rewrite above) rather than
  # cargo's built-in fetcher, which ignores them.
  home.sessionVariables.CARGO_NET_GIT_FETCH_WITH_CLI = "true";

  # VSOCK relay to host's filtered D-Bus proxy + matching session var —
  # disabled while bringing up the basic VM. Re-enable alongside the
  # dbus-vm-proxy / dbus-vm-vsock services in hyprland.nix.
  /*
    systemd.user.services.host-dbus-relay = {
      Unit = {
        Description = "Relay to host D-Bus notifications proxy over VSOCK";
        After = [ "default.target" ];
      };
      Service = {
        ExecStart = "${pkgs.socat}/bin/socat UNIX-LISTEN:%t/host-bus,fork,reuseaddr VSOCK-CONNECT:2:9999";
        Restart = "on-failure";
        RestartSec = 2;
      };
      Install.WantedBy = [ "default.target" ];
    };

    home.sessionVariables = {
      DBUS_SESSION_BUS_ADDRESS = "unix:path=$XDG_RUNTIME_DIR/host-bus";
    };
  */
}
