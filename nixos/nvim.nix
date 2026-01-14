{
  pkgs,
  pkgs-unstable,
  lib,
  inputs,
  ...
}: {
  imports = [inputs.nvf.homeManagerModules.default];
  programs.nvf = {
    enable = true;
    settings = import ./nvim-settings.nix {
      inherit pkgs pkgs-unstable lib inputs;
    };
  };
}
