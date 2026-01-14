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
    # Sops-nix for secrets management
    #sops-nix = {
    #  url = "github:Mic92/sops-nix";
    #  inputs.nixpkgs.follows = "nixpkgs";
    #};
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
    pkgs = import nixpkgs {inherit system;};
    pkgs-unstable = import nixpkgs-unstable {inherit system;};
  in {
    # Replace `nixos` with your hostname
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {inherit inputs pkgs-unstable;};
      modules = [
        # Import the previous configuration.nix we used,
        # so the old configuration file still takes effect
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = {inherit inputs pkgs-unstable;};
          home-manager.users.sam = ./home.nix;
          home-manager.backupFileExtension = "bak";
        }
      ];
    };
  };
}
