{
  description = "NixOS flake config";

  inputs = {
    # NixOS official package source, using the nixos-25.11 branch
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    # Neovim flake
    nvf.url = "github:notashelf/nvf";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    flake-parts,
    nvf,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      # Systems we want to build for
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      perSystem = {
        system,
        pkgs,
        ...
      }: {
        packages = let
          pkgs-unstable = import nixpkgs-unstable {inherit system;};
        in {
          default =
            (nvf.lib.neovimConfiguration {
              inherit pkgs;

              extraSpecialArgs = {
                inherit inputs pkgs-unstable;
              };

              modules = [
                ./nvim.nix
              ];
            })
        .neovim;
        };
      };
    };
}
