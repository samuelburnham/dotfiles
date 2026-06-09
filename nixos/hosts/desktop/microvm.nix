# Hosts the `dev` microvm where ghostty + tmux + claude + builds run.
# Host keeps Hyprland + a kitty escape-hatch; everything inside ghostty
# crosses the cloud-hypervisor boundary.
{
  inputs,
  pkgs,
  config,
  lib,
  username,
  ...
}:
let
  vmName = "dev";
  guestUid = 1000;
  bridgeIface = "br-microvm";
  bridgeAddr = "10.0.0.1";
  bridgePrefix = 24;
  vmAddr = "10.0.0.2";
  vmMac = "02:00:00:00:00:02";
  # Host's primary NIC — receives MASQUERADE'd outbound traffic from the VM.
  externalIface = "enp13s0";
in
{
  imports = [ inputs.microvm.nixosModules.host ];

  # Bridge with no slave interfaces; VM TAPs attach via microvm.interfaces.
  networking.bridges.${bridgeIface}.interfaces = [ ];
  networking.interfaces.${bridgeIface}.ipv4.addresses = [
    {
      address = bridgeAddr;
      prefixLength = bridgePrefix;
    }
  ];

  # NAT outbound for the VM subnet.
  networking.nat = {
    enable = true;
    enableIPv6 = false;
    internalInterfaces = [ bridgeIface ];
    externalInterface = externalIface;
  };

  # Allow DHCP + DNS replies from dnsmasq on the bridge.
  networking.firewall.interfaces.${bridgeIface} = {
    allowedUDPPorts = [
      53
      67
    ];
    allowedTCPPorts = [ 53 ];
  };

  # DHCP + DNS for the VM behind the bridge.
  services.dnsmasq = {
    enable = true;
    settings = {
      interface = bridgeIface;
      bind-interfaces = true;
      dhcp-range = [ "10.0.0.10,10.0.0.100,12h" ];
      dhcp-host = [ "${vmMac},${vmAddr}" ];
      # Forward upstream queries via the host's resolver
      server = [
        "1.1.1.1"
        "8.8.8.8"
      ];
    };
  };

  microvm.autostart = [ vmName ];

  # Make microvm@ services Type=notify: cloud-hypervisor forwards the
  # guest's sd_notify(READY) over the vsock, so the unit only reaches
  # "started" once the VM has actually booted. This lets the vsock-perms
  # ExecStartPost below run a single chmod (the socket provably exists by
  # then) instead of polling. Requires every microvm on this host to send
  # readiness — our NixOS guest's systemd does.
  microvm.host.useNotifySockets = true;

  # Slight CPU priority drop for the VM so host UI processes win when both
  # compete. Default weight is 100; 80 yields ~20% more host cycles under
  # contention. VM perf at idle host is unaffected.
  #
  # cloud-hypervisor creates notify.vsock owner-only (0700); group-rw lets
  # the kvm group ssh-over-VSOCK without sudo. With Type=notify (above) the
  # socket exists by the time ExecStartPost runs, so a single chmod does
  # it. Runs as the microvm service user, which owns the socket.
  systemd.services."microvm@${vmName}".serviceConfig = {
    CPUWeight = 80;
    ExecStartPost = [
      "${pkgs.coreutils}/bin/chmod g+rw /var/lib/microvms/${vmName}/notify.vsock"
    ];
  };

  microvm.vms.${vmName} =
    let
      system = "x86_64-linux";
      vmPkgs-unstable = import inputs.nixpkgs-unstable { inherit system; };
      vmPkgs-master = import inputs.nixpkgs-master {
        inherit system;
        config.allowUnfree = true;
      };
    in
    {
      # A host rebuild updates this VM's `current` runner but leaves the
      # running guest alone. Without this, a fully-declarative microvm
      # defaults to restartIfChanged = true, so any host rebuild that
      # changes the guest closure bounces the VM — and a cold boot here is
      # ~50s because the guest faults its whole closure in over the virtiofs
      # store. The store is shared read-only from the host, so a changed
      # closure is already visible inside the VM; apply it live with
      # `switch-to-configuration switch` (no reboot) or restart by hand when
      # a kernel/hardware-shaped field below changes.
      restartIfChanged = false;

      pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      specialArgs = {
        inherit
          inputs
          username
          ;
        pkgs-unstable = vmPkgs-unstable;
        pkgs-master = vmPkgs-master;
      };

      config =
        {
          config,
          pkgs,
          lib,
          username,
          ...
        }:
        {
          imports = [ inputs.home-manager.nixosModules.home-manager ];

          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "bak";
          home-manager.extraSpecialArgs = {
            inherit inputs username;
            pkgs-unstable = vmPkgs-unstable;
            pkgs-master = vmPkgs-master;
          };
          home-manager.users.${username} = import ../../home/sam/vm.nix;

          networking.hostName = "dev-vm";
          system.stateVersion = "25.11";

          # Match the host so commit timestamps and logs read in local time
          # with UTF-8 (the VM doesn't import common/system.nix).
          time.timeZone = "America/New_York";
          i18n.defaultLocale = "en_US.UTF-8";

          microvm = {
            hypervisor = "cloud-hypervisor";
            # All 24 host logical CPUs visible to the guest. CPUWeight on the
            # microvm@dev unit lets host UI processes outbid VM threads when
            # both compete.
            vcpu = 24;
            # Ceiling, not a reservation — KVM only backs pages the guest
            # actually dirties, so this costs nothing when the VM is idle.
            # 56 GiB of the host's ~61 GiB usable, leaving ~5 GiB floor for the
            # compositor, the cloud-hypervisor process itself, and a light
            # browser. balloon + free-page-reporting return unused VM RAM to
            # the host under pressure; the 16 GiB host swapfile cushions the
            # case where the VM holds most of it (heavy ZK proving) while host
            # apps also want memory.
            mem = 57344;
            balloon = true;

            # CID 2 is the host; 3 is the first guest.
            vsock.cid = 3;
            vsock.ssh.enable = true;

            shares = [
              {
                tag = "ro-store";
                source = "/nix/store";
                mountPoint = "/nix/.ro-store";
                proto = "virtiofs";
              }
              {
                # Only the code is shared from the host — so host-side rebuild,
                # git, restic backup, and inspection all see the same files.
                # Mounted over the persistent /home volume below. Everything
                # else in the VM home (cargo cache, claude state, tmux/direnv
                # data, history) lives in that volume, VM-private: it persists
                # across reboots, stays out of the host backup, and is not
                # reachable by a host-side breach of the VM.
                tag = "repos";
                source = "/home/${username}/repos";
                mountPoint = "/home/${username}/repos";
                proto = "virtiofs";
              }
            ];

            # Writable overlay so guest-side nix builds don't punch back into
            # the host store. Image lives under /var/lib/microvms/dev/.
            writableStoreOverlay = "/nix/.rw-store";
            volumes = [
              {
                # Writable layer over the shared ro host store — holds only
                # paths built/fetched in the VM that aren't already on the
                # host (VM nix builds, devshell closures, flake fetches). Big
                # ceiling for heavy ZK/Rust/Lean toolchains; sparse, so it only
                # consumes what's actually written.
                image = "nix-store-overlay.img";
                mountPoint = "/nix/.rw-store";
                size = 96 * 1024;
              }
              {
                # Persistent VM home — all home state (cargo, claude, tmux,
                # direnv, history, caches) survives reboots here. ~/repos is
                # virtiofs-mounted over the top from the host. Sparse image
                # (truncate), so the 64G is a ceiling, not upfront usage.
                image = "home.img";
                mountPoint = "/home";
                size = 64 * 1024;
              }
              # Persistent Nix DB (/nix/var) — deferred. The store overlay
              # already persists the paths; this would also persist the DB so
              # nix doesn't re-register/re-fetch them after a reboot. Needs the
              # nix-var-load-db service below (also commented) because microvm's
              # boot-time load-db runs before this volume mounts. Re-enable both
              # together if reboot re-fetching becomes a real cost.
              /*
                {
                  image = "nix-var.img";
                  mountPoint = "/nix/var";
                  size = 8 * 1024;
                }
              */
            ];

            interfaces = [
              {
                type = "tap";
                id = "vm-${vmName}";
                mac = vmMac;
              }
            ];

            # microvm creates the host-side TAP but doesn't bridge it. Enslave
            # it to br-microvm (created on the host) so the guest's NIC has an
            # L2 path to the bridge IP (10.0.0.1) — and thus to dnsmasq (DNS)
            # and NAT (outbound internet). Without this the TAP is dangling:
            # ssh still works (VSOCK, not IP), but DNS and `nix build` fail.
            binScripts.tap-up = lib.mkAfter ''
              ${pkgs.iproute2}/bin/ip link set dev 'vm-${vmName}' master '${bridgeIface}'
            '';
          };

          users.users.${username} = {
            isNormalUser = true;
            uid = guestUid;
            extraGroups = [ "wheel" ];
            shell = pkgs.bash;
            # SSH from the host into the VM uses these keys over VSOCK.
            openssh.authorizedKeys.keys = [
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBgsFjvY/ARgqDyf8kikQXPOaH7or10sTA2jsgR+vWNk sam@dev-vm"
            ];
          };
          users.users.root.hashedPassword = "!";
          security.sudo.wheelNeedsPassword = false;

          services.openssh.enable = true;

          # On a fresh /home volume nothing under it exists yet, and tmpfiles
          # creates implicit parents as root — so own /home/sam and the
          # home-manager profile chain explicitly (HM's profile lives under
          # ~/.local/state/nix/profiles). The ~/repos mountpoint for the
          # virtiofs share is listed too. Everything else under home is created
          # lazily by the user/tools and persists in the volume.
          systemd.tmpfiles.rules = [
            "d /home/${username}                            0700 ${username} users -"
            "d /home/${username}/.local                     0755 ${username} users -"
            "d /home/${username}/.local/state               0755 ${username} users -"
            "d /home/${username}/.local/state/nix           0755 ${username} users -"
            "d /home/${username}/.local/state/nix/profiles  0755 ${username} users -"
            "d /home/${username}/repos                       0755 ${username} users -"
          ];

          networking.useDHCP = lib.mkDefault true;
          # No guest firewall: the VM sits on a host-only bridge with
          # outbound-only NAT and no port forwards, so nothing off the host
          # can reach it. The firewall would only block host→VM access to dev
          # servers (e.g. a web server on 0.0.0.0 reached from host Firefox).
          networking.firewall.enable = false;

          nix.settings = {
            experimental-features = [
              "nix-command"
              "flakes"
            ];
            trusted-users = [ "@wheel" ];
          };

          # Scheduled GC to bound the store overlay. The keep-outputs/
          # keep-derivations options are passed here rather than set globally,
          # so they apply ONLY to this weekly job: it reclaims old generations
          # and genuinely-unreachable paths but spares the build-time deps of
          # rooted outputs (the large ZK/Rust/Lean toolchains). A manual
          # `nix-collect-garbage -d` keeps neither (nix.conf defaults), so it's
          # the aggressive full reclaim with no extra flags.
          nix.gc = {
            automatic = true;
            dates = "weekly";
            options = "--delete-older-than 14d --option keep-outputs true --option keep-derivations true";
          };

          # Pairs with the deferred /nix/var volume above — re-registers the
          # system closure into the persistent DB after the volume mounts
          # (microvm's boot-time load-db runs too early). Re-enable both
          # together.
          /*
            systemd.services.nix-var-load-db = {
              description = "Register the system closure into the persistent Nix DB";
              wantedBy = [ "multi-user.target" ];
              after = [ "nix-var.mount" ];
              requires = [ "nix-var.mount" ];
              before = [ "home-manager-sam.service" ];
              serviceConfig = {
                Type = "oneshot";
                RemainAfterExit = true;
                ExecStart = pkgs.writeShellScript "nix-var-load-db" ''
                  if [[ "$(cat /proc/cmdline)" =~ regInfo=([^ ]*) ]]; then
                    ${config.nix.package.out}/bin/nix-store --load-db < "''${BASH_REMATCH[1]}"
                  fi
                '';
              };
            };
          */

          # GH_TOKEN (gh api) and NIX_CONFIG (private flake-input
          # access-tokens) are forwarded per-session over ssh from the host,
          # which holds the sops-decrypted values. The VM stores no copy.
          services.openssh.settings.AcceptEnv = "GH_TOKEN NIX_CONFIG";

          environment.systemPackages = with pkgs; [
            git
            vim
            jq
            # Ghostty's terminfo entry only (not the GUI terminal), so the
            # xterm-ghostty TERM forwarded over ssh from the host terminal is
            # understood here — otherwise less/git-pager/vim report "terminal
            # not fully functional". From unstable to match the host's ghostty
            # (already built, shared store → no extra build).
            vmPkgs-unstable.ghostty.terminfo
          ];
        };
    };
}
