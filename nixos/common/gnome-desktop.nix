# GNOME desktop-manager — system-level bits for hosts that run a full
# GNOME session (currently only the laptop). Sister file to ./hyprland-desktop.nix;
# whichever a host imports decides which graphical environment is offered
# in GDM. system.nix keeps GDM, X server, fonts, audio, etc. shared.
{
  pkgs,
  ...
}:
{
  services.desktopManager.gnome.enable = true;
  services.gnome.games.enable = false;

  environment.gnome.excludePackages = with pkgs; [
    gnome-calendar
    epiphany
    geary
    gnome-music
  ];
}
