# GNOME workstation overlay — imported by NixOS hosts that run a full
# GNOME desktop (see hosts/desktop, hosts/laptop). Bundles the GNOME
# dconf/extensions config, GUI applications, Firefox profile, Ghostty
# terminal, GNOME-app MIME defaults, the local-path sandboxed nvim
# wrapper (hot-reload against ~/repos/dotfiles/apps), and rootless
# Podman. Ubuntu-style remote hosts don't import this.
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
      pkgs-unstable.nvme-cli # Currently v2.15
      restic
      nerd-fonts.fira-code
      nerd-fonts.jetbrains-mono
      # Image file utilities
      imagemagick
      ghostscript
      inkscape
      # Search backend for the pop-shell launcher (Super+/); the extension
      # alone is just the UI and returns nothing without this daemon.
      pop-launcher
      # `nvim` = the sandboxed editor from the standalone apps flake.
      # Each invocation resolves the current ~/repos/dotfiles/apps flake state,
      # so changes to plugins/config take effect without any profile upgrade.
      # System editor ($EDITOR) stays as plain `vim` for git commit messages etc.
      # Uses the local repo path; ubuntu host points at the github copy instead.
      (pkgs.writeShellScriptBin "nvim" ''
        exec ${pkgs.nix}/bin/nix run ${config.home.homeDirectory}/repos/dotfiles/apps#nvim -- "$@"
      '')
    ]
    ++ (with pkgs.gnomeExtensions; [
      pop-shell
      caffeine
      # Official Gnome top bar display for CPU, RAM, swap, and network usage
      system-monitor
      # Comprehensive top bar display for system info, I just use it for temps and fan speed
      vitals
      # Start apps in a specific workspace
      auto-move-windows
    ]);

  # Shell alias rather than a writeShellScriptBin wrapper because
  # `pkgs-master.claude-code` (base.nix) already provides ~/.nix-profile/bin/claude
  # and home-manager errors on a duplicate `claude` from a wrapper. The alias
  # only fires in interactive shells, so scripts (e.g. wt's commit-message
  # generator in base.nix) and other subprocesses still get the bare host
  # binary — matches the threat model: sandbox the *interactive* sessions you
  # launch yourself, leave one-shot tooling alone.
  programs.bash.shellAliases.claude =
    "nix run ${config.home.homeDirectory}/repos/dotfiles/apps#claude --";

  # Get file with searchable terminal output using Ctrl+Shift+J
  programs.ghostty = {
    enable = true;
    settings = {
      theme = "dark:iTerm2 Solarized Dark,light:iTerm2 Solarized Light";
      shell-integration-features = "no-cursor";
      cursor-style = "bar";
      # New shells start in ~/repos rather than $HOME. Matters because
      # boxvim/boxclaude refuse to launch from $HOME (would shadow the
      # per-subdir bind overlays with a wholesale home mount).
      working-directory = "${config.home.homeDirectory}/repos";
      # CSI u sequence (\e[13;2u = Shift+Enter under fixterms/kitty
      # keyboard protocol) survives tmux's `extended-keys on` passthrough,
      # which a raw `\n` byte does not — tmux silently drops the raw form.
      keybind = "shift+enter=text:\\x1b[13;2u";
    };
  };

  # Default apps
  # Firefox for web browser
  # Loupe for image viewer
  # Nautilus for file browser
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "image/png" = [ "org.gnome.Loupe.desktop" ];
      "image/jpg" = [ "org.gnome.Loupe.desktop" ];
      "image/gif" = [ "org.gnome.Loupe.desktop" ];
    };
  };

  services.podman = {
    enable = true;
  };

  # These settings can be found in `dconf-editor` or by running `dconf watch /` and then
  # editing GUI settings, which will print values in the terminal.
  # Each host must set dconf "org/gnome/shell".enabled-extensions separately.
  dconf = {
    enable = true;
    settings = {
      "org/gnome/shell/extensions/pop-shell" = {
        tile-by-default = true;
        active-hint = true;
        # Translucent blue accent — subtle in both light and dark mode.
        hint-color-rgba = "rgba(53, 132, 228, 0.4)";
        focus-down = [ "<Super>j" ];
        focus-left = [ "<Super>h" ];
        focus-right = [ "<Super>l" ];
        focus-up = [ "<Super>k" ];
        tile-enter = [ "<Super>BackSpace" ];
        tile-move-down = [ "<Shift>j" ];
        tile-move-left = [ "<Shift>h" ];
        tile-move-right = [ "<Shift>l" ];
        tile-move-up = [ "<Shift>k" ];
        tile-resize-down = [ "j" ];
        tile-resize-left = [ "h" ];
        tile-resize-right = [ "l" ];
        tile-resize-up = [ "k" ];
        tile-swap-down = [ "<Primary>j" ];
        tile-swap-left = [ "<Primary>h" ];
        tile-swap-right = [ "<Primary>l" ];
        tile-swap-up = [ "<Primary>k" ];
        # Pop-shell's tree-aware cross-monitor move; the GNOME-native
        # move-to-monitor-* equivalents fight the auto-tiler (window
        # snaps back to its old monitor) so they're cleared below.
        pop-monitor-up = [ "<Shift><Super>Up" ];
        pop-monitor-down = [ "<Shift><Super>Down" ];
        pop-monitor-left = [ ];
        pop-monitor-right = [ ];
        pop-workspace-down = [ ];
        pop-workspace-up = [ ];
      };
      "org/gnome/desktop/wm/keybindings" = {
        # Cleared in favor of pop-monitor-up/down above (pop-shell tree-aware).
        move-to-monitor-down = [ ];
        move-to-monitor-up = [ ];
        # TODO: This is set but not working on laptop
        move-to-workspace-left = [ "<Shift><Super>Left" ];
        move-to-workspace-right = [ "<Shift><Super>Right" ];
        switch-to-workspace-left = [ "<Super>Left" ];
        switch-to-workspace-right = [ "<Super>Right" ];
        switch-to-workspace-1 = [ "<Super>Home" ];
        switch-to-workspace-last = [ "<Super>End" ];
        # TODO: Make this Alt-Tab on laptop
        switch-windows = [ "<Alt>f" ];
        switch-windows-backward = [ "<Shift><Alt>f" ];
        maximize = [ "<Super>Up" ];
        unmaximize = [ "<Super>Down" ];
        #close = ["<Shift><Control>w"];
        toggle-fullscreen = [ "<Alt><Super>f" ];
        activate-window-menu = [ "<Super>equal" ];
        # Freed for pop-shell focus-left (<Super>h).
        minimize = [ ];
      };
      "org/gnome/shell/keybindings" = {
        toggle-application-view = [ "<Super>Tab" ];
      };
      # Cleared so <Super>Left/Right reach switch-to-workspace-left/right
      # instead of mutter's half-screen snap (the default).
      "org/gnome/mutter/keybindings" = {
        toggle-tiled-left = [ ];
        toggle-tiled-right = [ ];
      };
      "org/gnome/mutter" = {
        dynamic-workspaces = false;
        workspaces-only-on-primary = false;
      };
      "org/gnome/desktop/wm/preferences" = {
        num-workspaces = 4;
      };
      # TODO: org.gnome.settings-daemon.plugins.media-keys for play/pause & volume control
      "org/gnome/settings-daemon/plugins/media-keys" = {
        custom-keybindings = [
          "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
        ];
        # Freed for pop-shell focus-right (<Super>l).
        screensaver = [ ];
      };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
        binding = "<Control><Alt>t";
        command = "ghostty";
        name = "Launch Terminal";
      };
      "org/gnome/shell" = {
        favorite-apps = [
          "com.mitchellh.ghostty.desktop"
          "firefox.desktop"
          "org.gnome.Nautilus.desktop"
          "zulip.desktop"
          "spotify.desktop"
        ];
      };
      "org/gnome/desktop/interface" = {
        clock-format = "12h";
        color-scheme = "prefer-dark";
      };
      "org/gtk/settings/file-chooser" = {
        clock-format = "12h";
      };
      "org/gnome/desktop/sound" = {
        event-sounds = false;
      };
      "org/gnome/settings-daemon/plugins/color" = {
        night-light-enabled = true;
      };
      # "org/gnome/desktop/notifications/application/spotify" = {
      #   enable = false;
      # };
      # TODO: Add weather in "org/gnome/Weather/locations" and/or "org/gnome/shell/weather/locations"
    };
  };
}
