{
  description = "NixOS flake config";

  inputs = {
    # NixOS official package source, using the nixos-25.11 branch
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    # Tracks master for fast-moving leaf packages (e.g. claude-code) that
    # should update independently of the unstable channel
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    flake-parts.url = "github:hercules-ci/flake-parts";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # worktrunk moves fast — features like first-class alias dispatch only
    # landed in 0.39 and nixpkgs lags (currently 0.37). Track upstream main
    # so `nix flake update worktrunk` always pulls the latest.
    worktrunk = {
      url = "github:max-sixty/worktrunk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # nixpkgs-unstable lags the hyprshell release cycle; fzf-style matching
    # for desktop-file exec fields landed in 4.10.1 and isn't in unstable yet.
    # Track upstream directly; `nix flake update hyprshell` pulls the latest.
    hyprshell = {
      url = "github:H3rmt/hyprshell";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-unstable,
      nixpkgs-master,
      home-manager,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      perSystem =
        { pkgs, ... }:
        {
          # `nix fmt` entry point. nixfmt-tree = treefmt pre-configured with
          # nixfmt (RFC 166), respects .gitignore, caches per-file, parallel.
          formatter = pkgs.nixfmt-tree;

          # Surface the HM activation derivation as a package + runnable app
          # so `nix build .#ubuntu` and `nix run .#ubuntu` work without
          # needing a home-manager CLI install on the target.
          packages.ubuntu = self.homeConfigurations.ubuntu.activationPackage;

          apps.ubuntu = {
            type = "app";
            program = "${self.homeConfigurations.ubuntu.activationPackage}/activate";
          };
        };

      # nixosConfigurations / homeConfigurations are flake-level outputs, not
      # per-system, so they live in `flake` rather than `perSystem`. Shared
      # derivations (pkgs-unstable, pkgs-master, homeManagerModule) stay in
      # this `let` since they're only consumed here.
      flake =
        let
          system = "x86_64-linux";
          # Single source of truth for the primary user's login name.
          # Threaded through specialArgs (NixOS) and extraSpecialArgs
          # (home-manager) so every module derives paths and user-account
          # settings from it. The ubuntu standalone HM config below
          # overrides this to "ubuntu".
          username = "sam";
          pkgs-unstable = import nixpkgs-unstable { inherit system; };
          pkgs-master = import nixpkgs-master {
            inherit system;
            config.allowUnfree = true;
          };

          # Shared home-manager module config used by all hosts
          homeManagerModule = hostHome: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              inherit
                inputs
                pkgs-unstable
                pkgs-master
                username
                ;
            };
            home-manager.users.${username} = hostHome;
            home-manager.backupFileExtension = "bak";
          };
        in
        {
          nixosConfigurations = {
            # Desktop — AMD, Gigabyte B650I
            nixos = nixpkgs.lib.nixosSystem {
              inherit system;
              specialArgs = { inherit inputs pkgs-unstable username; };
              modules = [
                ./hosts/desktop/default.nix
                home-manager.nixosModules.home-manager
                (homeManagerModule ./home/sam/desktop.nix)
              ];
            };

            # Laptop — Intel, MSI
            nixbook = nixpkgs.lib.nixosSystem {
              inherit system;
              specialArgs = { inherit inputs pkgs-unstable username; };
              modules = [
                ./hosts/laptop/default.nix
                home-manager.nixosModules.home-manager
                (homeManagerModule ./home/sam/laptop.nix)
              ];
            };
          };

          # Standalone home-manager — for non-NixOS machines where we install
          # Nix and activate HM without a NixOS system layer (e.g. Ubuntu AMI
          # provisioned via terraform-server). Activate on the target with:
          #   nix run github:samuelburnham/dotfiles?dir=nixos#ubuntu
          homeConfigurations.ubuntu = home-manager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
            extraSpecialArgs = {
              inherit inputs pkgs-unstable pkgs-master;
              username = "ubuntu";
            };
            modules = [ ./home/sam/ubuntu.nix ];
          };
        };
    };
}
