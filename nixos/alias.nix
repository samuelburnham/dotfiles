{pkgs, ...}: let
  rebuild = pkgs.writeShellApplication {
    name = "rebuild";
    text = "nixos-rebuild switch --sudo";
  };
in {
  home.packages = [
    rebuild
  ];
}
