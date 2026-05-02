# GNOME workstation overlay — imported by NixOS hosts that run a full
# GNOME desktop (see hosts/desktop, hosts/laptop). Bundles the GNOME
# dconf/extensions config, GUI applications, Firefox profile, Ghostty
# terminal, GNOME-app MIME defaults, the local-path sandboxed nvim
# wrapper (hot-reload against ~/repos/dotfiles/nvim), and rootless
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
      # `nvim` = the sandboxed editor from the standalone nvim flake.
      # Each invocation resolves the current ~/repos/dotfiles/nvim flake state,
      # so changes to plugins/config take effect without any profile upgrade.
      # System editor ($EDITOR) stays as plain `vim` for git commit messages etc.
      # Uses the local repo path; ubuntu host points at the github copy instead.
      (pkgs.writeShellScriptBin "nvim" ''
        exec ${pkgs.nix}/bin/nix run ${config.home.homeDirectory}/repos/dotfiles/nvim#nvim -- "$@"
      '')
    ]
    ++ (with pkgs.gnomeExtensions; [
      tiling-shell
      caffeine
      # Official Gnome top bar display for CPU, RAM, swap, and network usage
      system-monitor
      # Comprehensive top bar display for system info, I just use it for temps and fan speed
      vitals
      # Start apps in a specific workspace
      auto-move-windows
    ]);

  # Get file with searchable terminal output using Ctrl+Shift+J
  programs.ghostty = {
    enable = true;
    settings = {
      theme = "dark:iTerm2 Solarized Dark,light:iTerm2 Solarized Light";
      shell-integration-features = "no-cursor";
      cursor-style = "bar";
      keybind = "shift+enter=text:\\n"; # Fixes Claude Code Shift+Enter newlines
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
      # TODO: Configure the selected layouts, though it will vary by monitors
      "org/gnome/shell/extensions/tilingshell" = {
        move-window-down = [ "<Super>x" ];
        move-window-left = [ "<Super>a" ];
        move-window-right = [ "<Super>d" ];
        move-window-up = [ "<Super>w" ];
        # TODO: Customize layouts, match to number of fixed workspaces
        selected-layouts = [
          [
            "Layout 4"
            "Layout 3"
          ]
          [
            "Layout 4"
            "Layout 3"
          ]
          [
            "Layout 4"
            "Layout 3"
          ]
          [
            "Layout 4"
            "Layout 3"
          ]
        ];
      };
      "org/gnome/desktop/wm/keybindings" = {
        move-to-monitor-down = [ "<Shift><Super>x" ];
        move-to-monitor-up = [ "<Shift><Super>w" ];
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
      };
      "org/gnome/shell/keybindings" = {
        toggle-application-view = [ "<Super>Tab" ];
      };
      # TODO: Integrate this with tiling-shell, or toggle 50/50 layout
      "org/gnome/mutter/keybindings" = {
        toggle-tiled-left = [ "<Shift><Super>h" ];
        toggle-tiled-right = [ "<Shift><Super>l" ];
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
      };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
        binding = "<Control><Alt>t";
        command = "ghostty --working-directory=${config.home.homeDirectory}/repos";
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
