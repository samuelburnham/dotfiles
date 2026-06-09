# sam@desktop — AMD, Gigabyte B650I
{
  pkgs,
  inputs,
  username,
  ...
}:
{
  imports = [
    ./base.nix
    ./alias.nix
    # Hyprland is the primary session; GNOME is imported alongside as a
    # fallback so logging into the GNOME entry in GDM gives the full
    # pop-shell setup (keybinds, favourites, dconf) rather than a stripped
    # session. The two overlays set several of the same options
    # (programs.ghostty, services.podman, xdg.mimeApps) to identical
    # values, which Nix merges cleanly; home.packages overlaps duplicate
    # harmlessly in the list. Drop ./gnome.nix once Hyprland is settled
    # to halve the build closure.
    ./hyprland.nix
    ./gnome.nix
  ];

  home.username = username;
  home.homeDirectory = "/home/${username}";

  # ssh dev-vm → ssh-over-VSOCK into the dev microvm as sam. systemd ships
  # an ssh_config snippet that ProxyCommands `vsock-mux/*` hosts through
  # systemd-ssh-proxy, but it only matches when that literal pattern is the
  # host argument — an alias with HostName set doesn't trigger it, so the
  # ProxyCommand is replicated here against the same helper.
  programs.ssh = {
    enable = true;
    # HM's implicit `Host *` defaults are deprecated. They only restated
    # ssh's own built-in defaults, so opt out and keep ssh's defaults
    # rather than re-declaring an empty block.
    enableDefaultConfig = false;
    matchBlocks.dev-vm = {
      hostname = "vsock-mux/var/lib/microvms/dev/notify.vsock";
      user = "sam";
      proxyCommand = "${pkgs.systemd}/lib/systemd/systemd-ssh-proxy %h %p";
      # Forward the sops-decrypted tokens from the host session into the VM
      # (sshd there sets AcceptEnv GH_TOKEN NIX_CONFIG). Only present when
      # the launching shell has them — see the login-shell wrapper on the
      # Super+T bind / launcher entry.
      sendEnv = [
        "GH_TOKEN"
        "NIX_CONFIG"
      ];
      extraOptions = {
        ProxyUseFdpass = "yes";
        CheckHostIP = "no";
        StrictHostKeyChecking = "no";
        UserKnownHostsFile = "/dev/null";
        # /dev/null known-hosts means ssh re-"adds" the key every connect
        # and prints a warning each time; ERROR drops that notice while
        # still surfacing real failures.
        LogLevel = "ERROR";
      };
    };
  };

  # sops = {
  #   # It's also possible to use a ssh key, but only when it has no password:
  #   #age.sshKeyPaths = [ "/home/user/path-to-ssh-key" ];
  #   defaultSopsFile = ./secrets/secrets.yaml;
  #   defaultSopsFormat = "yaml";
  #
  #   age = {
  #     keyFile = "/home/sam/.config/sops/age/keys.txt"; # must have no password!
  #   };
  #
  #   secrets = {
  #     anthropic-api-key = {};
  #     #"myservice/my_subdir/my_secret" = {};
  #   };
  #   #secrets.test = {
  #   # sopsFile = ./secrets.yml.enc; # optionally define per-secret files
  #
  #   # %r gets replaced with a runtime directory, use %% to specify a '%'
  #   # sign. Runtime dir is $XDG_RUNTIME_DIR on linux and $(getconf
  #   # DARWIN_USER_TEMP_DIR) on darwin.
  #   #  path = "%r/test.txt";
  #   #};
  # };

  # GNOME: Set enabled-extensions for desktop (shared extensions only).
  # Inert under Hyprland — dconf state without gnome-shell to consume it.
  dconf.settings = {
    "org/gnome/shell" = {
      enabled-extensions = with pkgs.gnomeExtensions; [
        pop-shell.extensionUuid
        caffeine.extensionUuid
        system-monitor.extensionUuid
        vitals.extensionUuid
        auto-move-windows.extensionUuid
      ];
    };
  };
}
