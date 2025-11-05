# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the Grub2 boot loader with EFI.
  boot.loader.systemd-boot.enable = false;
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    useOSProber = true;
    devices = ["nodev"];
  };
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot";
  };

  # Fixes suspend issue on Gigabyte B650I motherboard
  # Note: If DDR5 RAM XMP profile is enabled, resuming from suspend may fail
  # I noticed this once in the NixOS boot log: `bug: bad page state in process swapper`
  # If so, lower the RAM speed in BIOS incrementally and test. E.g. 6400Mhz might fail, but 6000Mhz works
  boot.kernelParams = ["acpi_osi=\"!Windows 2015\""];
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
    wantedBy = ["multi-user.target"];
  };
  # Enable wakeup for Kinesis keyboard
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="usb", ATTRS{idVendor}=="29ea", ATTRS{idProduct}=="0362", ATTR{power/wakeup}="enabled"
  '';

  networking.hostName = "nixos"; # Define your hostname.
  #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager = {
    enable = true;
    wifi = {
      # Neither of these solved my problem of Wifi having poor connection for several minutes after resuming from suspend
      # Solution: use Ethernet
      scanRandMacAddress = false;
      powersave = false;
    };
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  # Not set explicitly but Wayland is enabled and the default
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  services.gnome.games.enable = false;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;
  # Had to remove and re-add printer in Gnome settings after adding the driver
  services.printing.drivers = [pkgs.brlaser];

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sam = {
    isNormalUser = true;
    description = "Sam";
    extraGroups = ["networkmanager" "wheel"];
    packages = with pkgs; [
      #  thunderbird
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  nix.settings.experimental-features = ["nix-command" "flakes"];

  nix.settings.trusted-users = ["sam"];

  #programs.firefox.enable = true; # Managed by home-manager

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    git
  ];

  environment.gnome.excludePackages = with pkgs; [
    gnome-calendar
    epiphany
    geary
    gnome-music
  ];

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = false;
  };

  environment.variables.EDITOR = "nvim";

  # Increase `sudo` timeout to 30 minutes
  security.sudo.extraConfig = "Defaults timestamp_timeout=30";

  # Automated backups to external drive
  systemd.services.restic-backup = {
    enable = true; #TODO: Is this needed?
    description = "Restic backup";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = ''
        ${pkgs.restic}/bin/restic backup /home/sam/dotfiles --password-file /home/sam/restic-password
      '';
      EnvironmentFile = "/home/sam/restic.env";
    };
  };
  systemd.timers.restic-backup = {
    description = "Run backup daily";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "*:0/5";
      Persistent = true;
    };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
