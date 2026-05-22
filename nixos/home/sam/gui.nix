# Shared GUI overlay — common bits imported by both DE-specific overlays
# (./gnome.nix and ./hyprland.nix). Holds the desktop applications,
# Ghostty terminal, image-MIME defaults, Podman, Firefox profile, and
# the local-path sandboxed nvim wrapper that behave the same regardless
# of which compositor is active. DE-specific packages (pop-shell,
# gnome-control-center, waybar, etc.) stay in their respective overlay.
#
# When both overlays are imported on the same host (desktop, where GNOME
# stays around as a fallback), this file is included twice — Nix's
# module system dedupes by path, so shared options only get defined once.
{
  pkgs,
  pkgs-unstable,
  config,
  ...
}:
{
  imports = [
    ./firefox.nix
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
      # `nvim` = the sandboxed editor from the standalone apps flake.
      # Each invocation resolves the current ~/repos/dotfiles/apps flake state,
      # so changes to plugins/config take effect without any profile upgrade.
      # System editor ($EDITOR) stays as plain `vim` for git commit messages etc.
      # Uses the local repo path; ubuntu host points at the github copy instead.
      (pkgs.writeShellScriptBin "nvim" ''
        exec ${pkgs.nix}/bin/nix run ${config.home.homeDirectory}/repos/dotfiles/apps#nvim -- "$@"
      '')
    ];

  # Shell alias rather than a writeShellScriptBin wrapper because
  # `pkgs-master.claude-code` (base.nix) already provides ~/.nix-profile/bin/claude
  # and home-manager errors on a duplicate `claude` from a wrapper. The alias
  # only fires in interactive shells, so scripts (e.g. wt's commit-message
  # generator in base.nix) and other subprocesses still get the bare host
  # binary — matches the threat model: sandbox the *interactive* sessions you
  # launch yourself, leave one-shot tooling alone.
  programs.bash.shellAliases.claude = "nix run ${config.home.homeDirectory}/repos/dotfiles/apps#claude --";

  # Get file with searchable terminal output using Ctrl+Shift+J
  programs.ghostty = {
    enable = true;
    settings = {
      # Nerd Font so Waybar/neovim/tmux glyphs (e.g. file-icon plugins,
      # devicons, powerline) render instead of blank tofu. Package
      # installed via home.packages above.
      font-family = "FiraCode Nerd Font Mono";
      theme = "dark:Catppuccin Mocha,light:Catppuccin Latte";
      shell-integration-features = "no-cursor";
      cursor-style = "bar";
      mouse-hide-while-typing = true;
      # New shells start in ~/repos rather than $HOME. Matters because
      # boxvim/boxclaude refuse to launch from $HOME (would shadow the
      # per-subdir bind overlays with a wholesale home mount).
      working-directory = "${config.home.homeDirectory}/repos";
      # CSI u sequence (\e[13;2u = Shift+Enter under fixterms/kitty
      # keyboard protocol) survives tmux's `extended-keys on` passthrough,
      # which a raw `\n` byte does not — tmux silently drops the raw form.
      keybind = "shift+enter=text:\\x1b[13;2u";
      # Allow terminal apps to read clipboard via OSC 52 without a
      # per-request dialog. Default is "ask", which silently fails when
      # the dialog is dismissed before Claude Code's checkImage command
      # exits. Claude Code also uses wl-paste directly, but some code
      # paths fall back to OSC 52.
      clipboard-read = "allow";
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
