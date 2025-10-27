{pkgs, ...}: let
  rebuild = pkgs.writeShellApplication {
    name = "rebuild";
    text = "nixos-rebuild switch --use-remote-sudo";
  };
in {
  home.packages = [
    rebuild
  ];
}
