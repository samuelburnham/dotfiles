{pkgs, ...}: {
  home.packages = with pkgs.gnomeExtensions; [
    tiling-shell
    caffeine
    # Official Gnome top bar display for CPU, RAM, swap, and network usage
    system-monitor
    # Comprehensive top bar display for system info, I just use it for temps and fan speed
    vitals
    # Start apps in a specific workspace
    auto-move-windows
    # Use Gnome/`powerprofilesctl` Performance mode on AC charging, Balanced mode on battery
    auto-power-profile
  ];
  # These settings can be found in `dconf-editor` or by running `dconf watch /` and then
  # editing GUI settings, which will print values in the terminal.
  dconf = {
    enable = true;
    settings = {
      "org/gnome/shell" = {
        enabled-extensions = with pkgs.gnomeExtensions; [
          caffeine.extensionUuid
          vitals.extensionUuid
          system-monitor.extensionUuid
          tiling-shell.extensionUuid
          auto-move-windows.extensionUuid
          auto-power-profile.extensionUuid
        ];
      };
      # TODO: Configure the selected layouts, though it will vary by monitors
      "org/gnome/shell/extensions/tilingshell" = {
        move-window-down = ["<Super>x"];
        move-window-left = ["<Super>a"];
        move-window-right = ["<Super>d"];
        move-window-up = ["<Super>w"];
        # TODO: Customize layouts, match to number of fixed workspaces
        selected-layouts = [
          ["Layout 4" "Layout 3"]
          ["Layout 4" "Layout 3"]
          ["Layout 4" "Layout 3"]
          ["Layout 4" "Layout 3"]
        ];
      };
      "org/gnome/desktop/wm/keybindings" = {
        move-to-monitor-down = ["<Shift><Super>x"];
        move-to-monitor-up = ["<Shift><Super>w"];
        # TODO: This is set but not working on laptop
        move-to-workspace-left = ["<Shift><Super>Left"];
        move-to-workspace-right = ["<Shift><Super>Right"];
        switch-to-workspace-left = ["<Super>Left"];
        switch-to-workspace-right = ["<Super>Right"];
        switch-to-workspace-1 = ["<Super>Home"];
        switch-to-workspace-last = ["<Super>End"];
        # TODO: Make this Alt-Tab on laptop
        switch-windows = ["<Alt>f"];
        switch-windows-backward = ["<Shift><Alt>f"];
        maximize = ["<Super>Up"];
        unmaximize = ["<Super>Down"];
        close = ["<Shift><Control>w"];
        toggle-fullscreen = ["<Alt><Super>f"];
      };
      "org/gnome/shell/keybindings" = {
        toggle-application-view = ["<Super>Tab"];
      };
      # TODO: Integrate this with tiling-shell, or toggle 50/50 layout
      "org/gnome/mutter/keybindings" = {
        toggle-tiled-left = ["<Shift><Super>h"];
        toggle-tiled-right = ["<Shift><Super>l"];
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
      "org/gnome/desktop/notifications/application/spotify" = {
        enable = false;
      };
      # TODO: Add weather in "org/gnome/Weather/locations" and/or "org/gnome/shell/weather/locations"
    };
  };
}
