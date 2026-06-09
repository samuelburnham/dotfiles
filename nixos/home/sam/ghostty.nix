# Ghostty terminal config — the host-native terminal. Super+T opens it
# already ssh'd into the dev microvm (ssh-dev-vm); plain ghostty is a host
# shell. Kept in its own file (imported by gui.nix) rather than inlined so
# the config is easy to find and reuse.
{ config, ... }:
{
  programs.ghostty = {
    enable = true;
    settings = {
      font-family = "FiraCode Nerd Font Mono";
      theme = "dark:Catppuccin Mocha,light:Catppuccin Latte";
      shell-integration-features = "no-cursor";
      cursor-style = "bar";
      mouse-hide-while-typing = true;
      working-directory = "${config.home.homeDirectory}/repos";
      # CSI u sequence (\e[13;2u = Shift+Enter under fixterms/kitty
      # keyboard protocol) survives tmux's `extended-keys on` passthrough,
      # which a raw `\n` byte does not — tmux silently drops the raw form.
      keybind = "shift+enter=text:\\x1b[13;2u";
      # OSC 52 clipboard reads without a per-request dialog. Default
      # "ask" silently fails when the dialog is dismissed before the
      # invoking command exits.
      clipboard-read = "allow";
    };
  };
}
