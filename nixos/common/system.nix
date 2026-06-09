# Shared NixOS system configuration
# Imported by all hosts
{
  config,
  pkgs,
  inputs,
  username,
  ...
}:
{
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];
  # TODO: Add an option to enter the BIOS from the boot loader,
  # rather than remembering which key to spam on startup
  # Use the Grub2 boot loader with EFI.
  boot.loader.systemd-boot.enable = false;
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    useOSProber = true;
    devices = [ "nodev" ];
  };
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot";
  };

  boot.enableContainers = true;
  virtualisation.containers.enable = true;

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
  # Not set explicitly but Wayland is enabled and the default
  services.xserver.enable = true;

  # GDM works as the display manager for both GNOME and Hyprland sessions;
  # which desktop is offered is decided by which of ./gnome-desktop.nix or
  # ./hyprland-desktop.nix the host imports.
  services.displayManager.gdm.enable = true;

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

  services.gnome.games.enable = false;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with 'passwd'.
  users.users.${username} = {
    isNormalUser = true;
    description = "Sam Burnham";
    extraGroups = [
      "networkmanager"
      "wheel"
      # microvm host runs cloud-hypervisor as microvm:kvm; this lets the
      # user reach /var/lib/microvms/*/notify.vsock for ssh-over-VSOCK.
      "kvm"
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    trusted-users = [ username ];
  };

  # Secrets management with sops-nix
  # Note: When setting up a new NixOS device, get the Age private key from password manager.
  # Then run:
  # ```
  # mkdir -p ~/.config/sops/age
  # vim ~/.config/sops/age/keys.txt
  # ```
  # Then paste the contents, save, and rebuild NixOS
  # Check encrypted file by opening `~/dotfiles/nixos/secrets/secrets.yaml`
  # Open decrypted file by running `sops ~/dotfiles/nixos/secrets/secrets.yaml`
  sops.defaultSopsFile = ../secrets/secrets.yaml;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = "/home/${username}/.config/sops/age/keys.txt";

  # owner = sam so the user shell can read it to export NIX_CONFIG (for
  # forwarding to the dev microvm); the nix daemon reads it as root anyway.
  sops.secrets.nix-access-tokens.owner = username;

  # gh PAT, read by the user's shell rc and exported as GH_TOKEN.
  sops.secrets.gh-token = {
    mode = "0400";
    owner = username;
  };

  # AWS + GCP credentials for Terraform and the cloud CLIs, read by the user's
  # shell rc and exported (see base.nix). aws-* are the access-key pair;
  # gcp-credentials is a service-account JSON that GOOGLE_APPLICATION_CREDENTIALS
  # points at.
  sops.secrets.aws-access-key-id = {
    mode = "0400";
    owner = username;
  };
  sops.secrets.aws-secret-access-key = {
    mode = "0400";
    owner = username;
  };
  # GCP disabled for now — no service-account key yet. Re-enable once
  # `gcp-credentials` is added to secrets.yaml (and the export in base.nix).
  # sops.secrets.gcp-credentials = {
  #   mode = "0400";
  #   owner = username;
  # };

  # GitHub PAT for authenticated nix fetches — decrypted at runtime by sops-nix
  # This allows fetching private GitHub flake inputs with `github:org/name` URLs
  nix.extraOptions = ''
    !include ${config.sops.secrets.nix-access-tokens.path}
  '';

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    git
    sops
    usbutils
  ];

  environment.gnome.excludePackages = with pkgs; [
    gnome-calendar
    epiphany
    geary
    gnome-music
  ];

  environment.variables.EDITOR = "vim";

  # Increase `sudo` timeout to 30 minutes
  security.sudo.extraConfig = "Defaults timestamp_timeout=30";

  programs.steam.enable = false;

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

  networking.firewall.enable = true;
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
