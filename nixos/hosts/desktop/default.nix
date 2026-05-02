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
    ../../common/system.nix
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

  # WiFi workarounds — neither solved poor connection after resuming from suspend
  # Solution: use Ethernet
  # networking.networkmanager.wifi = {
  #   scanRandMacAddress = false;
  #   powersave = false;
  # };

  # TODO: Fix printing once new CUPS version is release
  # Had to remove and re-add printer in Gnome settings after adding the driver
  services.printing.drivers = [ pkgs.brlaser ];

  # Automated backups to external drive
  systemd.services.restic-backup = {
    enable = true; # TODO: Is this needed?
    description = "Restic backup";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = ''
        ${pkgs.restic}/bin/restic backup /home/${username}/repos/dotfiles --password-file /home/${username}/restic-password
      '';
      EnvironmentFile = "/home/${username}/restic.env";
    };
  };
  systemd.timers.restic-backup = {
    description = "Run backup daily";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*:0/5";
      Persistent = true;
    };
  };
}
