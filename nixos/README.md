# NixOS Config

## Install

- Run `nix-shell -p vim -p git`

- Run `sudo vim /etc/nixos/configuration.nix` to enable flakes:
```
# /etc/nixos/configuration.nix

# ...
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
# ...
```
- Confirm the settings with `sudo nixos-rebuild switch`

- Run `git clone https://github.com/samuelburnham/dotfiles.git` and `cd dotfiles/nixos`

- Run `cp /etc/nixos/hardware-configuration.nix .` to get the local hardware config

- Make sure the hostname in `/etc/nixos/configuration.nix` matches that in `./flake.nix` and `./configuration.nix`

- Run `sudo mv /etc/nixos /etc/nixos.bak` to back up the existing config

- Run `sudo ln -s ~/dotfiles/nixos /etc/nixos` to symlink local path so we can track changes in Git and edit without sudo

- Run `sudo nixos-rebuild switch`

- Remove `/etc/nixos.bak` if desired

Sources:
[Enable Nix flakes](https://nixos-and-flakes.thiscute.world/nixos-with-flakes/nixos-with-flakes-enabled#enable-nix-flakes)
[Managing config with git](https://nixos-and-flakes.thiscute.world/nixos-with-flakes/other-useful-tips#managing-the-configuration-with-git)
