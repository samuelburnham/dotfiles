{
  pkgs,
  config,
  ...
}:
let
  # If you ever move the flake (e.g. relocated the dotfiles repo), the
  # `rebuild` binary in the currently-active profile still bakes in the
  # OLD path and will fail with "No such file or directory". Bootstrap
  # the new path once by running the underlying command directly:
  #   nixos-rebuild switch --flake /new/path/to/nixos --sudo
  # After that switch completes, the regenerated `rebuild` script picks
  # up the updated path from this file and works on subsequent runs.
  rebuild = pkgs.writeShellApplication {
    name = "rebuild";
    text = "nixos-rebuild switch --flake ${config.home.homeDirectory}/repos/dotfiles/nixos --sudo";
  };
in
{
  home.packages = [
    rebuild
  ];
}
