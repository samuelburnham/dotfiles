# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];
  # TODO: Add an option to enter the BIOS from the boot loader,
  # rather than remembering which key to spam on startup
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
  boot.kernelParams = ["mem_sleep_default=s2idle" "resume_offset=14559232"];
  # UUID of root ext4 partition
  boot.resumeDevice = "/dev/disk/by-uuid/e1746389-93c2-4f21-8086-f3b5e685413b";

  # Enable sleep settings with systemd and logind
  powerManagement.enable = true;
  # Enable performance profiles used by Gnome
  services.power-profiles-daemon.enable = true;
  # Suspend first then hibernate when closing the lid
  services.logind = {
    lidSwitch = "suspend-then-hibernate";
    settings.Login = {
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
  # Alternatively, can try setting the max charge via TLP. Not a priority atm

  networking.hostName = "nixbook"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

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
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

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
    description = "Sam Burnham";
    extraGroups = ["networkmanager" "wheel"];
  };

  nix.settings.trusted-users = ["sam"];

  nix.settings.experimental-features = ["nix-command" "flakes"];

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    git
  ];

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = false;
  };

  environment.variables.EDITOR = "nvim";

  # Increase `sudo` timeout to 30 minutes
  security.sudo.extraConfig = "Defaults timestamp_timeout=30";

  # TODO: Make laptop-specific config with below
  # Also add battery percentage to top bar in Gnome

  # Remap Caps Lock to Esc on tap, Ctrl on hold/chord
  # Not useful for Kinesis keyboard
  # From https://discourse.nixos.org/t/best-way-to-remap-caps-lock-to-esc-with-wayland/39707/6
  services.interception-tools = let
    itools = pkgs.interception-tools;
    itools-caps = pkgs.interception-tools-plugins.caps2esc;
  in {
    enable = true;
    plugins = [itools-caps];
    # requires explicit paths: https://github.com/NixOS/nixpkgs/issues/126681
    udevmonConfig = pkgs.lib.mkDefault ''
      - JOB: "${itools}/bin/intercept -g $DEVNODE | ${itools-caps}/bin/caps2esc -m 1 | ${itools}/bin/uinput -d $DEVNODE"
        DEVICE:
          EVENTS:
            EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
    '';
  };

  programs.steam.enable = true;

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
