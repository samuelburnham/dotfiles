# Main home-manager config
{ config, pkgs, inputs, ... }:

{
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
  ];

  programs.bash = {
    enable = true;
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
