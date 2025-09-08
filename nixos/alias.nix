{
  pkgs,
  ...
}:
let
  rebuild = pkgs.writeShellApplication {
    name = "rebuild";
    text = "sudo nixos-rebuild switch";
  };
in
{
  home.packages = [
    rebuild
  ];
}

