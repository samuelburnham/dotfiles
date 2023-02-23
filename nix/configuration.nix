# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the Grub2 boot loader with EFI.
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    version = 2;
    useOSProber = true;
    devices = [ "nodev" ];
  };
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot";
  };

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlo1.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };
  nix = {
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings = {
      keep-outputs = true;
      trusted-substituters = [ "https://lean4.cachix.org/" ];
      trusted-public-keys = [ "lean4.cachix.org-1:mawtxSxcaiWE24xCXXgh3qnvlTkyU7evRRnGeAhD4Wk=" ];
    }; 
  };

  # Enable the X11 windowing system.
  services = {

    openssh.enable = true;

    printing.enable = true;

    xserver = {
      enable = true;
      libinput.enable = true;
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = false;
      desktopManager.gnome.enable = true;
      layout = "us";
      videoDrivers = [ "nvidia" ];
    };
  };

  # Potential Xorg fix
  # systemd.services.display-manager.restartIfChanged = false;

  # Enable XMonad
  # services.xserver.displayManager.defaultSession = "none+xmonad";
  # services.xserver.windowManager = {
  #   xmonad.enable = true;
  #   xmonad.enableContribAndExtras = true;
  #   xmonad.extraPackages = hpkgs: [
  #     hpkgs.xmonad
  #     hpkgs.xmonad-contrib
  #     hpkgs.xmonad-extras
  #   ];
  # };

  # Exclude GNOME Packages
  environment.gnome.excludePackages = [ pkgs.gnome.geary pkgs.epiphany ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };

  hardware.bluetooth = {
    enable = true;
    settings = { General = { Enable = "Source,Sink,Media,Socket"; };};
    powerOnBoot = true;
  };

  swapDevices = [
    { device = "/dev/disk/by-label/swap"; }
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sam = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  nixpkgs.config.allowUnfree = true;
  programs.steam.enable = true;
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    git
    wget
    firefox
    xmobar
    nitrogen
    picom
    dmenu
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

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
  system.stateVersion = "21.11"; # Did you read the comment?

}

