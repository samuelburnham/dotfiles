# Hyprland desktop — system-level bits for hosts that run Hyprland instead
# of GNOME (currently only the desktop). Sister file to ./gnome-desktop.nix;
# whichever a host imports decides which graphical environment GDM offers.
# Per the NixOS Hyprland wiki page, `programs.hyprland.enable` already wires
# up polkit, xdg-desktop-portal-hyprland, graphics drivers, fonts, dconf,
# xwayland, and the GDM session entry, so this file stays small.
{
  pkgs,
  pkgs-unstable,
  ...
}:
{
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;

    # Hyprland moves fast — tracking nixpkgs-unstable instead of 25.11. The
    # portal package must match the Hyprland version (the IPC between them
    # is not API-stable across minor releases), hence both come from
    # pkgs-unstable. The corresponding home-manager module in
    # home/sam/hyprland.nix sets `package = null; portalPackage = null;`
    # so it picks these up rather than introducing a third version.
    package = pkgs-unstable.hyprland;
    portalPackage = pkgs-unstable.xdg-desktop-portal-hyprland;
  };

  # Bluetooth — GNOME enabled this transitively; Hyprland does not.
  # Needed for the blueman-applet tray icon and Settings panel.
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # gvfs powers Nautilus' trash, recent files, network mounts, and MTP
  # support. GNOME enables it implicitly; under Hyprland we set it directly.
  services.gvfs.enable = true;

  # gnome-keyring stores secrets for Bitwarden/browsers etc. GDM unlocks the
  # default keyring at login via PAM when this is enabled.
  services.gnome.gnome-keyring.enable = true;

  # Hint Electron/Chromium apps (Slack, Discord, VS Code, Chrome) to use
  # Wayland natively — without this they capture via XWayland and screen
  # sharing breaks. Documented at:
  # https://nixos.wiki/wiki/Hyprland and
  # https://wiki.hypr.land/Useful-Utilities/Screen-Sharing/
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # XDG portal — `programs.hyprland.enable` already registers
  # xdg-desktop-portal-hyprland. The Hyprland portal handles ScreenCast and
  # Screenshot; the gtk portal handles FileChooser and AppChooser. Listing
  # the order silences the "no default portal" warning that nixpkgs prints
  # on activation. The gtk portal comes from `pkgs` (stable) — it has no
  # version coupling with Hyprland.
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = [
      "hyprland"
      "gtk"
    ];
  };
}
