# sam@laptop — Intel, MSI
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

  home.packages = with pkgs; [
    thunderbird
    # Gaming overlay for FPS, temps, CPU/GPU load
    mangohud
    # Use Gnome/`powerprofilesctl` Performance mode on AC charging, Balanced mode on battery
    gnomeExtensions.auto-power-profile
    # Enable hibernate, suspend-to-hibernate, and reboot to BIOS in Gnome power menu. When finished with the laptop for the day/overnight, hibernate or suspend-to-hibernate will allow resuming progress later. Simply closing the lid and unplugging any USB peripherals will also suspend-to-hibernate per systemd settings in configuration.nix
    gnomeExtensions.power-off-options
  ];

  # GNOME: Set enabled-extensions for laptop (shared + laptop-specific)
  dconf.settings = {
    "org/gnome/shell" = {
      enabled-extensions = with pkgs.gnomeExtensions; [
        pop-shell.extensionUuid
        caffeine.extensionUuid
        system-monitor.extensionUuid
        vitals.extensionUuid
        auto-move-windows.extensionUuid
        auto-power-profile.extensionUuid
        power-off-options.extensionUuid
      ];
    };
    # Laptop-specific keybind
    "org/gnome/desktop/wm/keybindings" = {
      close = [ "<Shift><Control>w" ];
    };
    "org/gnome/desktop/notifications/application/spotify" = {
      enable = false;
    };
  };
}
