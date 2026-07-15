# worktrunk — git worktree switcher whose commit generation shells out to
# `claude` (see claude.nix). Kept out of base.nix so it stays off the
# bare-metal workstations: worktree-based dev (and the claude it needs for
# commit messages) happens only in the isolated environments. Imported by the
# dev microvm (dev-vm.nix) and the Ubuntu box (ubuntu.nix), alongside claude.nix.
{
  inputs,
  ...
}:
{
  imports = [
    inputs.worktrunk.homeModules.default
  ];

  # worktrunk — upstream's home-manager module installs the package and
  # wires `eval "$(wt config shell init bash)"` into bash.initExtra. Hooks
  # and aliases still live in ~/.config/worktrunk/config.toml below.
  programs.worktrunk = {
    enable = true;
    enableBashIntegration = true;
  };

  # worktrunk — `wt switch --create <branch>` spins up a new worktree as a
  # sibling dir (default template) and fires this pre-start hook, which
  # creates a dedicated tmux session for it and drops the current client
  # into it. `wt remove` / `wt merge` reverse the whole thing.
  home.file.".config/worktrunk/config.toml".text = ''
    [pre-start]
    tmux = """
    # Worktrunk's default template creates sibling worktrees as
    # `<repo>.<branch>`, so basename yields e.g. `dotfiles.test`. tmux's
    # target-spec grammar always parses `.` as a `session.pane`
    # separator — even inside a `=exact` prefix — which is unfixable at
    # lookup time. Strip dots at the source so creation and lookup both
    # see a bare session name. `tr . -` also covers dotted branch names
    # like `v1.2.3`; worktrunk only sanitizes slashes, not dots.
    S=$(basename "{{ worktree_path }}" | tr . -)
    tmux new-session -d -s "$S" -c "{{ worktree_path }}"
    # Guarded on $TMUX so calls from outside tmux are a no-op instead
    # of erroring on "no current client".
    [ -n "$TMUX" ] && tmux switch-client -t "=$S"
    """
    # Guarded on .envrc presence so worktrees in non-direnv repos don't
    # error. `direnv allow` accepts a path and resolves the .envrc itself.
    direnv = """
    [ -f "{{ worktree_path }}/.envrc" ] && direnv allow "{{ worktree_path }}" || true
    """

    [pre-remove]
    tmux = """
    # Must match pre-start's transform so we target the session that
    # was actually created. Error-swallow preserved: kill-session is a
    # no-op if the session doesn't exist, and we don't want `wt remove`
    # to fail on cleanup of a missing session.
    S=$(basename "{{ worktree_path }}" | tr . -)
    tmux kill-session -t "=$S" 2>/dev/null || true
    """

    # Short aliases. Aliases run as shell commands (not as wt subcommand
    # names), so the `wt` prefix is explicit. `{{ args }}` forwards
    # positional args verbatim with shell-safe escaping.
    #
    # `--no-cd` on the two switch variants suppresses worktrunk's shell-cd
    # directive: the pre-start tmux hook already lands us in a session
    # whose cwd is the new worktree, so sourcing a `cd` in the *original*
    # shell would only drag the session we just left into the new dir —
    # surprising when we swap back to it later. Leaving the flag off the
    # plain `wt switch` / `wt switch --create` commands means the rare
    # out-of-tmux invocation still gets shell-follow behavior by default.
    [aliases]
    s = "wt switch --no-cd {{ args }}"
    c = "wt switch --create --no-cd {{ args }}"
    m = "wt merge {{ args }}"
    r = "wt remove {{ args }}"
    l = "wt list {{ args }}"

    # Sonnet over upstream's haiku default: commits are load-bearing
    # history and branch diffs can run thousands of lines, where
    # haiku's summarization drops important context. The rest of the
    # flags strip Claude Code's normal scaffolding (skills, CLAUDE.md,
    # tool loop, session write) so this behaves like a one-shot API
    # call rather than an interactive agent run.
    [commit.generation]
    command = "CLAUDECODE= MAX_THINKING_TOKENS=0 claude -p --no-session-persistence --model=sonnet --tools=''' --disable-slash-commands --setting-sources=''' --system-prompt='''"
  '';
}
