# Laptop-specific NixOS system configuration
# MSI Intel laptop
{
  config,
  pkgs,
  ...
}:
{
  imports = [
    ../../common/system.nix
    # Laptop stays on GNOME (the desktop host migrated to Hyprland). The
    # GNOME system-level options moved out of common/system.nix so each
    # host opts in to its own graphical environment.
    ../../common/gnome-desktop.nix
    ./hardware-configuration.nix
  ];

  # Renaming this breaks `rebuild` until bootstrapped: `nixos-rebuild
  # switch` resolves `nixosConfigurations.<hostname>` by default, so a
  # new name here must be mirrored in the flake's `nixosConfigurations`
  # key (nixos/flake.nix) in the same commit. After the rename, run
  # the first switch with the new key explicit, e.g.
  #   nixos-rebuild switch --flake /home/sam/repos/dotfiles/nixos#newname --sudo
  # (or run `sudo hostname newname` first so the default lookup hits
  # the new key). Subsequent `rebuild` invocations work normally.
  networking.hostName = "nixbook";

  # TODO: Add battery percentage to top bar in Gnome
  # TODO: Switch to Hyprland
  # TODO: Add an external-drive Steam library folder — internal SSD is
  # tight on space

  # Hibernate with swapfile, from https://nixos.wiki/wiki/Hibernation
  # Create swapfile for extra RAM while programming and also for hibernation
  # Make sure swap space >= RAM size
  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 32 * 1024;
    }
  ];
  # Suspend with s2idle for fast resume, then hibernate (suspend-to-disk) after 30 min for low power mode. Requires pressing the power button to wake up
  boot.kernelParams = [
    "mem_sleep_default=s2idle"
    "resume_offset=14559232"
  ];
  # UUID of root ext4 partition
  boot.resumeDevice = "/dev/disk/by-uuid/e1746389-93c2-4f21-8086-f3b5e685413b";

  # Enable sleep settings with systemd and logind
  powerManagement.enable = true;
  # Enable performance profiles used by Gnome
  services.power-profiles-daemon.enable = true;
  # Suspend first then hibernate when closing the lid
  services.logind = {
    settings.Login = {
      HandleLidSwitch = "suspend-then-hibernate";
      SuspendThenHibernate = "yes";
    };
  };
  # 30 minute time delay after suspend before hibernation
  systemd.sleep.extraConfig = ''
    AllowSuspendThenHibernate=yes
    HibernateDelaySec=30m
  '';

  # If using laptop as daily driver, consider setting max charge to 80% for battery health/longevity
  # My laptop doesn't have a `/sys/class/power_supply/BAT*/charge_control_{start,end}_threshold`, so UPower isn't able to provide max charge as a Gnome power settings option. Could try adding support for my laptop to https://github.com/BeardOverflow/msi-ec or checking back in a few months
  # See https://vdwaa.nl/gnome-upower-charge-thresholds.html
  #services.upower.enable = true;

  # Remap Caps Lock to Esc on tap, Ctrl on hold/chord
  # Not useful for Kinesis keyboard (desktop), but great for laptop keyboard
  # From https://discourse.nixos.org/t/best-way-to-remap-caps-lock-to-esc-with-wayland/39707/6
  services.interception-tools =
    let
      itools = pkgs.interception-tools;
      itools-caps = pkgs.interception-tools-plugins.caps2esc;
    in
    {
      enable = true;
      plugins = [ itools-caps ];
      # requires explicit paths: https://github.com/NixOS/nixpkgs/issues/126681
      udevmonConfig = pkgs.lib.mkDefault ''
        - JOB: "${itools}/bin/intercept -g $DEVNODE | ${itools-caps}/bin/caps2esc -m 1 | ${itools}/bin/uinput -d $DEVNODE"
          DEVICE:
            EVENTS:
              EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
      '';
    };
}
