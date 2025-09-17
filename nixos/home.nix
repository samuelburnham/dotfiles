# Main home-manager config
{pkgs, ...}: {
  imports = [
    ./nvim.nix
    ./gnome.nix
    ./alias.nix
    ./firefox.nix
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
    cowsay
    wl-clipboard
    # TODO: Switch to nixpkgs-unstable once v2.15 comes out (currently on master only)
    # This fixes the broken Freon Gnome extension when `nvme-cli` is enabled
    nvme-cli
    restic
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    gh
    htop
    imagemagick
  ];

  programs.bash = {
    enable = true;
  };

  # Get file with searchable terminal output using Ctrl+Shift+J
  programs.ghostty = {
    enable = true;
    settings = {
      theme = "Builtin Solarized Dark";
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
    userName = "samuelburnham";
    userEmail = "45365069+samuelburnham@users.noreply.github.com";
    extraConfig = {
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
      "text/html" = ["firefox.desktop"];
      "text/xml" = ["firefox.desktop"];
      "x-scheme-handler/http" = ["firefox.desktop"];
      "x-scheme-handler/https" = ["firefox.desktop"];
      "application/pdf" = ["firefox.desktop"];
      "image/png" = ["org.gnome.Loupe.desktop"];
      "image/jpg" = ["org.gnome.Loupe.desktop"];
      "image/gif" = ["org.gnome.Loupe.desktop"];
    };
  };

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
