# Main home-manager config
{
  pkgs,
  pkgs-unstable,
  inputs,
  ...
}: {
  imports = [
    ./nvim.nix
    ./gnome.nix
    ./alias.nix
    ./firefox.nix
    #inputs.sops-nix.homeManagerModules.sops
  ];

  home.username = "sam";
  home.homeDirectory = "/home/sam";

  home.packages = with pkgs; [
    bitwarden-desktop
    vscode
    zulip
    spotify
    obsidian
    telegram-desktop
    discord
    slack
    google-chrome
    todoist-electron
    libreoffice
    wl-clipboard
    ripgrep
    lm_sensors
    smartmontools
    # TODO: Freon Gnome extension is broken when `nvme-cli` is enabled
    # https://github.com/UshakovVasilii/gnome-shell-extension-freon/issues/293
    pkgs-unstable.nvme-cli # Currently v2.15
    restic
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    gh
    htop
    # Image file utilities
    imagemagick
    ghostscript
    inkscape
  ];

  programs.bash = {
    enable = true;
    # Changes backup files `ls` color to dim cyan, otherwise they are invisible with solarized dark theme
    bashrcExtra = ''
      LS_COLORS=$(echo "$LS_COLORS" | sed 's/=00;90/=36;2/g')
    '';
  };

  # Get file with searchable terminal output using Ctrl+Shift+J
  programs.ghostty = {
    enable = true;
    settings = {
      theme = "Builtin Solarized Dark";
      shell-integration-features = "no-cursor";
      cursor-style = "bar";
      keybind = "shift+enter=text:\\n"; # Fixes Claude Code Shift+Enter newlines
    };
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

  # Default apps
  # Firefox for web browser
  # Loupe for image viewer
  # Nautilus for file browser
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "image/png" = ["org.gnome.Loupe.desktop"];
      "image/jpg" = ["org.gnome.Loupe.desktop"];
      "image/gif" = ["org.gnome.Loupe.desktop"];
    };
  };

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
    #      #"${self.home.homeDirectory}/repos/argument/tests/templean:/workspace:rw"
    #      "/home/sam/repos/argument/tests/templean:/home/ubuntu:rw"
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

  #environment.sessionVariables.DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";

  # Enable Hyprland
  #programs.kitty.enable = true; # required for the default Hyprland config
  #wayland.windowManager.hyprland.enable = true; # enable Hyprland

  # Optional, hint Electron apps to use Wayland:
  # home.sessionVariables.NIXOS_OZONE_WL = "1";

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
