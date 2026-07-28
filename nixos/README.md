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

## Restore /home from backup

**Config comes from nix, state comes from restic.** `git clone` + `nixos-rebuild`
gives a working machine with empty data; restic fills in only what can't be
regenerated. `/home/sam` is backed up daily to the One Touch drive
(`services.restic.backups.onetouch`, repo `/mnt/onetouch/NixOS-restic`).

Home-manager owns most of `~` as `/nix/store` symlinks and recreates them on
activation, so **don't blanket-restore over `~`**: dropping a stale regular file
on top of an HM-managed path causes "would be clobbered" on the next switch.
Restore selectively instead.

### Prerequisites

- The One Touch drive connected.
- The sops age key. It normally lives at `~/.config/sops/age/keys.txt` — which is
  *inside* the backup, so a copy must be kept off-disk, or the restic password
  (below) can't be decrypted and the backup can't be opened.

### Steps

1. Rebuild from the flake first, so home-manager lays down its own dotfiles:
   ```
   git clone https://github.com/samuelburnham/dotfiles.git && cd dotfiles/nixos
   sudo nixos-rebuild switch --flake .#<host>
   ```
2. Get the repo password:
   ```
   sudo cat /run/secrets/restic-password                     # running system
   # fresh machine (secrets not decrypted yet):
   SOPS_AGE_KEY_FILE=/path/to/keys.txt \
     sops -d --extract '["restic-password"]' secrets/secrets.yaml
   ```
3. Mount the backup read-only and cherry-pick state back with the helper script.
   It mounts via FUSE (nothing is staged to disk); rsync only real data from
   another terminal — Documents, repos, `~/.local/share/<app>`, keys, browser
   profiles — and skip anything home-manager owns:
   ```
   export RESTIC_PASSWORD=...       # from step 2, if not on the live system
   ./scripts/restic-restore.sh      # Ctrl-C to unmount when done
   ```
4. On any conflict, delete the restored copy and let `nixos-rebuild switch` win —
   the declarative version is canonical.

### Not covered by this backup

restic only stores `/home/sam`, so these are not restored and must be set up
again: WiFi credentials (`/etc/NetworkManager/system-connections`), Bluetooth
pairings (`/var/lib/bluetooth`), the dev microvm's `home.img` under
`/var/lib/microvms/`, and the excluded `.cache` / Steam / `.local/share/containers`.
