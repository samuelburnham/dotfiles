# ubuntu — standalone home-manager config for a stock Ubuntu AMI with
# Nix installed (see terraform-server for provisioning). No NixOS, no
# GUI, no rebuild wrapper. Pulls nvim from the dotfiles/nvim flake as
# the unwrapped (no-nixpak) variant, since the sandbox binds NixOS-only
# paths (/run/current-system, /etc/profiles, ...) that don't exist here.
{
  pkgs,
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
    # Resolves the dotfiles/nvim flake on each invocation. First run
    # downloads + builds (slow); subsequent runs hit the /nix/store cache.
    # To update: `nix flake update` inside dotfiles/nvim and push, then
    # `nix run .#ubuntu` re-activates with the new lock.
    (pkgs.writeShellScriptBin "nvim" ''
      exec ${pkgs.nix}/bin/nix run github:samuelburnham/dotfiles?dir=nvim#nvim-unwrapped -- "$@"
    '')
  ];
}
