{
  description = "NixOS flake config";

  inputs = {
    # NixOS official package source, using the nixos-25.11 branch
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim = {
      url = "path:../nvim";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-unstable.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # TODO: Rewrite with flake-parts and/or Dendritic Nix
  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    neovim,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs-unstable = import nixpkgs-unstable {inherit system;};
    # Shared home-manager module config used by all hosts
    homeManagerModule = hostHome: {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.extraSpecialArgs = {inherit inputs pkgs-unstable;};
      home-manager.users.sam = hostHome;
      home-manager.backupFileExtension = "bak";
    };
  in {
    nixosConfigurations = {
      # Desktop — AMD, Gigabyte B650I
      nixos = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit inputs pkgs-unstable;};
        modules = [
          ./hosts/desktop/default.nix
          home-manager.nixosModules.home-manager
          (homeManagerModule ./hosts/desktop/home.nix)
        ];
      };

      # Laptop — Intel, MSI
      nixbook = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit inputs pkgs-unstable;};
        modules = [
          ./hosts/laptop/default.nix
          home-manager.nixosModules.home-manager
          (homeManagerModule ./hosts/laptop/home.nix)
        ];
      };
    };
  };
}
