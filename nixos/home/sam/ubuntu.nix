# ubuntu — standalone home-manager config for a stock Ubuntu AMI with
# Nix installed (see terraform-server for provisioning). No NixOS, no
# GUI, no rebuild wrapper.
{
  pkgs,
  inputs,
  username,
  ...
}:
{
  imports = [
    ./base.nix
  ];

  home.username = username;
  home.homeDirectory = "/home/${username}";

  home.packages = [
    inputs.self.packages.${pkgs.system}.nvim
  ];
}
