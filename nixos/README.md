# NixOS Config

## Install

- Connect to the Internet

- Run `nix-shell -p vim git`

- Run `git clone https://github.com/samuelburnham/dotfiles.git` and `cd dotfiles/nixos`

- Run `cp /etc/nixos/hardware-configuration.nix .` to get the local hardware config

- Make sure the current username and hostname in `/etc/nixos/configuration.nix` match those in `./flake.nix` and `./configuration.nix`

- Run `sudo mv /etc/nixos /etc/nixos.bak` to back up the existing config

- Run `sudo ln -s ~/dotfiles/nixos /etc/nixos` to symlink local path so we can track changes in Git and edit without sudo

- Run `sudo nixos-rebuild switch`

- Remove `/etc/nixos.bak` if desired

- Reboot into BIOS, then set NixOS-bootloader as the default / first option. Then boot and check the bootloader shows options for NixOS and available generations

- If there are major boot or login issues, reboot into a recovery terminal if available or a live USB with the NixOS installer. Then use `nixos-enter` to enter the broken system and fix the issue.

Sources:
[Enable Nix flakes](https://nixos-and-flakes.thiscute.world/nixos-with-flakes/nixos-with-flakes-enabled#enable-nix-flakes)
[Managing config with git](https://nixos-and-flakes.thiscute.world/nixos-with-flakes/other-useful-tips#managing-the-configuration-with-git)
