# Shared GUI overlay — common bits imported by both DE-specific overlays
# (./gnome.nix and ./hyprland.nix). Holds the desktop applications,
# kitty terminal, image-MIME defaults, Podman, Firefox profile, and the
# nvim package that behave the same regardless of which compositor is
# active. DE-specific packages (pop-shell, gnome-control-center, waybar,
# etc.) stay in their respective overlay.
#
# When both overlays are imported on the same host (desktop, where GNOME
# stays around as a fallback), this file is included twice — Nix's
# module system dedupes by path, so shared options only get defined once.
{
  pkgs,
  pkgs-unstable,
  inputs,
  config,
  ...
}:
{
  imports = [
    ./firefox.nix
    # Ghostty as the host-native terminal. vm.nix imports ghostty.nix
    # directly (not via this file), so the microvm guest keeps its own
    # copy; path-dedup means the host's two DE overlays share this one.
    ./ghostty.nix
  ];

  home.packages =
    with pkgs;
    [
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
      lm_sensors
      smartmontools
      # TODO: Freon Gnome extension is broken when `nvme-cli` is enabled
      # https://github.com/UshakovVasilii/gnome-shell-extension-freon/issues/293
      pkgs-unstable.nvme-cli
      restic
      nerd-fonts.fira-code
      nerd-fonts.jetbrains-mono
      imagemagick
      ghostscript
      inkscape
      # GNOME's image viewer; works fine standalone outside a GNOME session.
      loupe
    ]
    ++ [
      # nvim = the standalone nvf-built editor exposed as packages.nvim
      # at the nixos flake top level. System editor ($EDITOR) stays as
      # plain `vim` for git commit messages.
      inputs.self.packages.${pkgs.system}.nvim

      # Connect to the dev microvm, forwarding the sops-decrypted tokens so
      # `gh api` and private nix flake inputs work inside it. Reads the
      # secrets explicitly (rather than relying on an interactive shell)
      # so GUI launches — Hyprland keybind, app-launcher entry — carry them
      # too; ssh's SendEnv (see desktop.nix) does the forwarding.
      (pkgs.writeShellScriptBin "ssh-dev-vm" ''
        [ -r /run/secrets/gh-token ] && export GH_TOKEN="$(cat /run/secrets/gh-token)"
        [ -r /run/secrets/nix-access-tokens ] && export NIX_CONFIG="$(cat /run/secrets/nix-access-tokens)"
        exec ${pkgs.openssh}/bin/ssh dev-vm "$@"
      '')
    ];

  # App-launcher entry: opens host ghostty already ssh'd into the dev
  # microvm, so launching it drops straight into the VM shell. Plain
  # ghostty (no args) stays the host-native terminal.
  xdg.desktopEntries.ghostty-dev = {
    name = "Ghostty (dev VM)";
    genericName = "Terminal";
    comment = "Ghostty connected to the dev microvm over VSOCK";
    exec = "ghostty -e ssh-dev-vm";
    icon = "com.mitchellh.ghostty";
    terminal = false;
    categories = [
      "System"
      "TerminalEmulator"
    ];
  };

  # Host kitty — escape hatch for when the dev microvm is unhealthy or
  # when you need a terminal that isn't tied to the VM at all.
  programs.kitty = {
    enable = true;
    font = {
      name = "FiraCode Nerd Font Mono";
      size = 11;
    };
    themeFile = "Catppuccin-Mocha";
    settings = {
      cursor_shape = "beam";
      mouse_hide_wait = "1.0";
      enable_audio_bell = false;
      hide_window_decorations = "yes";
    };
  };

  # Image MIME defaults — Loupe for png/jpg/gif. File-manager defaults
  # (inode/directory) live in the DE-specific overlay since the GNOME
  # session already defaults to Nautilus and Hyprland explicitly sets it.
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "image/png" = [ "org.gnome.Loupe.desktop" ];
      "image/jpeg" = [ "org.gnome.Loupe.desktop" ];
      "image/gif" = [ "org.gnome.Loupe.desktop" ];
    };
  };

  services.podman = {
    enable = true;
  };
}
