{pkgs, ...}: let
  rebuild = pkgs.writeShellApplication {
    name = "rebuild";
    text = "nixos-rebuild switch --flake /home/sam/dotfiles/nixos --sudo";
  };
in {
  home.packages = [
    rebuild
  ];
}
