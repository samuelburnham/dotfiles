# ubuntu — standalone home-manager config for a stock Ubuntu AMI with
# Nix installed (see terraform-server for provisioning). No NixOS, no
# GUI, no rebuild wrapper.
{
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    ../modules/base.nix
    ../modules/claude.nix
    ../modules/codex.nix
    ../modules/worktrunk.nix
  ];

  # This config runs on a disposable cloud VM, so default to "auto" mode like
  # the dev microvm — Claude auto-approves safe actions and blocks risky ones
  # (see dev-vm.nix). Overrides base.nix's prompting default.
  programs.claude-code.settings.permissions.defaultMode = "auto";

  home.packages = [
    inputs.self.packages.${pkgs.system}.nvim
  ];
}
