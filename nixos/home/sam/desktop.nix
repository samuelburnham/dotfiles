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
    ./gnome.nix
  ];

  home.username = username;
  home.homeDirectory = "/home/${username}";

  services.podman = {
    enable = true;
    #builds = {
    #  claudeman = {
    #    # TODO: Set `SetWorkingDirectory` so I can use relative path here
    #    file = "/home/sam/dotfiles/nixos/Dockerfile.claudeman";
    #    autoStart = false;
    #    tags = ["latest"];
    #  };
    #};
    #containers = {
    #  claudeman = {
    #    image = "localhost/claudeman:latest";
    #    autoStart = false; # Don't start on boot. What about autostart on login?
    #    # Enable Docker compatibility
    #    dockerCompat.enable = true;
    #    #environment = {
    #    #  TERM = "xterm-256color";
    #    #  EDITOR = "vim";
    #    #};
    #    volumes = [
    #      #"${self.home.homeDirectory}/repos/tests/templean:/workspace:rw"
    #      "/home/sam/repos/tests/templean:/home/ubuntu:rw"
    #    ];

    #    # Add network access, SSH forwarding, etc
    #    extraPodmanArgs = [
    #      "--userns=keep-id"
    #      "--user ubuntu"
    #      "--workdir /home/ubuntu"
    #      #"--privileged"
    #      #"--network=host"
    #    ];
    #  };
    #};
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

  # GNOME: Set enabled-extensions for desktop (shared extensions only)
  dconf.settings = {
    "org/gnome/shell" = {
      enabled-extensions = with pkgs.gnomeExtensions; [
        tiling-shell.extensionUuid
        caffeine.extensionUuid
        system-monitor.extensionUuid
        vitals.extensionUuid
        auto-move-windows.extensionUuid
      ];
    };
  };
}
