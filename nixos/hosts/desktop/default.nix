# Desktop-specific NixOS system configuration
# Gigabyte B650I AMD desktop
{
  config,
  pkgs,
  inputs,
  username,
  ...
}:
{
  imports = [
    ../../common/host.nix
    # Desktop primarily runs Hyprland; the matching home-manager overlay
    # (home/modules/hyprland.nix) is wired via home/profiles/desktop.nix in
    # nixos/flake.nix. GNOME stays installed alongside as a fallback —
    # both desktop entries appear in GDM, pick whichever at login.
    ../../common/hyprland-wm.nix
    ../../common/gnome-de.nix
    # Root-owned /etc/claude-code/managed-settings.json — the enforced deny
    # policy for the host-side Claude (home/profiles/desktop.nix), which it
    # can't edit as an unprivileged user.
    ../../common/claude-managed-settings.nix
    ./hardware-configuration.nix
    ./microvm.nix
  ];

  # Renaming this breaks `rebuild` until bootstrapped: `nixos-rebuild
  # switch` resolves `nixosConfigurations.<hostname>` by default, so a
  # new name here must be mirrored in the flake's `nixosConfigurations`
  # key (nixos/flake.nix) in the same commit. After the rename, run
  # the first switch with the new key explicit, e.g.
  #   nixos-rebuild switch --flake /home/sam/repos/dotfiles/nixos#newname --sudo
  # (or run `sudo hostname newname` first so the default lookup hits
  # the new key). Subsequent `rebuild` invocations work normally.
  networking.hostName = "nixos"; # Define your hostname.
  #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Fixes suspend issue on Gigabyte B650I motherboard
  # Note: If DDR5 RAM XMP profile is enabled, resuming from suspend may fail
  # I noticed this once in the NixOS boot log: `bug: bad page state in process swapper`
  # If so, lower the RAM speed in BIOS incrementally and test. E.g. 6400Mhz might fail, but 6000Mhz usually works
  boot.kernelParams = [ "acpi_osi=\"!Windows 2015\"" ];
  systemd.services.disable-xh00-wakeup = {
    description = "Disable XH00 device wakeup";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "disable-xh00-wakeup" ''
        if grep -q "XH00.*enabled" /proc/acpi/wakeup; then
          echo "XH00" > /proc/acpi/wakeup
        fi
      '';
    };
    wantedBy = [ "multi-user.target" ];
  };
  # Enable wakeup for Kinesis keyboard
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="usb", ATTRS{idVendor}=="29ea", ATTRS{idProduct}=="0362", ATTR{power/wakeup}="enabled"
  '';

  # DDC/CI brightness control for the DisplayPort monitors. A desktop panel
  # has no backlight sysfs, so brightness is driven over the monitor's I2C
  # channel with ddcutil (VCP feature 0x10). hardware.i2c.enable loads
  # i2c-dev, creates /dev/i2c-*, and defines the i2c group; the user must be
  # in that group for non-root access. DDC/CI must also be enabled in the
  # monitor's own OSD menu for any of this to take effect.
  hardware.i2c.enable = true;
  users.users.${username}.extraGroups = [ "i2c" ];
  environment.systemPackages = [ pkgs.ddcutil ];

  # WiFi workarounds — neither solved poor connection after resuming from suspend
  # Solution: use Ethernet
  # networking.networkmanager.wifi = {
  #   scanRandMacAddress = false;
  #   powersave = false;
  # };

  # 16 GB swapfile as overflow under memory pressure.
  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 16 * 1024;
    }
  ];

  # Prefer reclaiming file-backed cache over anonymous memory.
  boot.kernel.sysctl."vm.swappiness" = 10;

  # TODO: Fix printing once new CUPS version is release
  # Had to remove and re-add printer in Gnome settings after adding the driver
  services.printing.drivers = [ pkgs.brlaser ];

  # Restic backups to the One Touch external USB drive.
  # The drive isn't always connected, so it's mounted via systemd automount:
  # nofail keeps boot from blocking on it, and x-systemd.automount defers the
  # mount until first access — e.g. when the backup timer fires. x-gvfs-show
  # keeps it visible in the Nautilus sidebar. Runs while it's absent fail, and
  # Persistent=true catches up on the next mount.
  fileSystems."/mnt/onetouch" = {
    device = "/dev/disk/by-uuid/5EAC-B331";
    fsType = "exfat";
    options = [
      "nofail"
      "x-systemd.automount"
      "x-systemd.idle-timeout=600"
      "x-gvfs-show"
      "uid=1000"
      "gid=100"
    ];
  };

  sops.secrets.restic-password = {
    mode = "0400";
    owner = username;
  };
  services.restic.backups.onetouch = {
    repository = "/mnt/onetouch/NixOS-restic";
    passwordFile = config.sops.secrets.restic-password.path;
    paths = [ "/home/${username}" ];
    exclude = [
      "/home/${username}/.cache"
      # Steam: game installs, caches, and local saves. Cloud-synced saves are
      # recoverable from Steam; anything only stored here is not backed up.
      "/home/${username}/.local/share/Steam"
      # Rootless podman image/container storage; images are re-pullable.
      "/home/${username}/.local/share/containers"
    ];
    timerConfig = {
      # Daily at noon
      OnCalendar = "*-*-* 12:00:00";
      Persistent = true;
    };
    pruneOpts = [
      "--keep-daily 7"
      "--keep-weekly 4"
      "--keep-monthly 6"
    ];
  };
  systemd.services.restic-backups-onetouch.unitConfig.RequiresMountsFor = "/mnt/onetouch";
}
