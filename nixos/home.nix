{ config, pkgs, ... }: 

{
  programs.home-manager.enable = true;
  programs.direnv = {
    enable = true;

    nix-direnv = {
      enable = true;
    };
  };
  home.username = "sam";
  home.homeDirectory = "/home/sam";
  home.stateVersion = "21.11";
  home.packages = with pkgs; [
    gnome.gnome-terminal
    gnome.gnome-tweaks
    gnome.gnome-shell-extensions
    gnomeExtensions.appindicator
    emacs
    chromium
    slack
    tdesktop
    thunderbird
    spotify
    discord
    zulip
    neovim
    xclip
    ghc
    stack
    cabal-install
    rustup
    bash
    zsh
    cabextract
    vulkan-tools
    ffmpeg
    r128gain
    deadbeef
    atomicparsley
    vscode
    elan
  ];
}

