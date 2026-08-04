# sam@laptop — Intel, MSI
{
  pkgs,
  gnomeBaseExtensions,
  ...
}:
{
  imports = [
    ../modules/base.nix
    ../modules/alias.nix
    ../modules/gnome.nix
    # The laptop is a daily driver where claude runs directly against real
    # repos, so it carries claude.nix (prompting default + bubblewrap sandbox
    # kept, same as the desktop host) and worktrunk, which shells out to claude
    # for commit messages.
    ../modules/claude.nix
    ../modules/worktrunk.nix
  ];

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
    "org/gnome/shell".enabled-extensions = map (e: e.extensionUuid) (
      gnomeBaseExtensions
      ++ (with pkgs.gnomeExtensions; [
        auto-power-profile
        power-off-options
      ])
    );
    # Laptop-specific keybind
    "org/gnome/desktop/wm/keybindings" = {
      close = [ "<Shift><Control>w" ];
    };
    "org/gnome/desktop/notifications/application/spotify" = {
      enable = false;
    };
  };
}
