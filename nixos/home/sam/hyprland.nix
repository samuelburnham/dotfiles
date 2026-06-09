# Hyprland workstation overlay — imported by hosts that run Hyprland
# instead of GNOME (see home/sam/desktop.nix). Sister file to ./gnome.nix.
# The two share GUI applications, Ghostty, MIME defaults, and Podman via
# ./gui.nix; this file holds only the Hyprland-specific bits.
{
  pkgs,
  pkgs-unstable,
  config,
  inputs,
  ...
}:
let
  # Send the focused window to "workspace N on the current monitor".
  # Hyprland's movetoworkspace arg is absolute, so this wrapper looks up
  # the active workspace ID (1-5 = DP-2 by our pinning rules; 6-10 =
  # HDMI-A-1) and adds 5 to N when the cursor is on the upper monitor.
  moveToWsRelative = pkgs.writeShellScript "hypr-move-ws-relative" ''
    set -eu
    n=$1
    cur=$(${pkgs.hyprland}/bin/hyprctl activeworkspace -j \
          | ${pkgs.jq}/bin/jq -r .id)
    if [ "$cur" -ge 6 ]; then
      target=$((n + 5))
    else
      target=$n
    fi
    ${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace "$target"
  '';

  # Focus the leftmost (`l`) or rightmost (`r`) mapped window on the
  # currently active workspace. Used by the Super+Shift+H/L binds — sister
  # to the Super+Shift+J/K `focusmonitor` jumps. Workspaces are pinned per
  # monitor, so filtering by workspace ID implicitly filters by monitor.
  focusEdgeWindow = pkgs.writeShellScript "hypr-focus-edge-window" ''
    set -eu
    dir=$1
    ws=$(${pkgs.hyprland}/bin/hyprctl activeworkspace -j \
         | ${pkgs.jq}/bin/jq -r .id)
    list=$(${pkgs.hyprland}/bin/hyprctl clients -j \
           | ${pkgs.jq}/bin/jq -r --argjson ws "$ws" \
               '.[] | select(.workspace.id == $ws and .mapped == true)
                    | "\(.at[0]) \(.address)"')
    target=""
    case "$dir" in
      l) target=$(printf '%s\n' "$list" | sort -n  | head -n1 | awk '{print $2}') ;;
      r) target=$(printf '%s\n' "$list" | sort -rn | head -n1 | awk '{print $2}') ;;
    esac
    if [ -n "$target" ]; then
      ${pkgs.hyprland}/bin/hyprctl dispatch focuswindow "address:$target"
    fi
  '';

  # Toggle the focused window in/out of the `magic` special workspace,
  # following it either way. In a normal workspace → move into the
  # scratchpad and show it. Already in the scratchpad → eject to the
  # normal workspace on the focused monitor.
  scratchToggle = pkgs.writeShellScript "hypr-scratch-toggle" ''
    set -eu
    ws=$(${pkgs.hyprland}/bin/hyprctl activewindow -j \
         | ${pkgs.jq}/bin/jq -r '.workspace.name')
    case "$ws" in
      special:*)
        target=$(${pkgs.hyprland}/bin/hyprctl monitors -j \
                 | ${pkgs.jq}/bin/jq -r '.[] | select(.focused == true) | .activeWorkspace.id')
        ${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace "$target"
        ;;
      *)
        ${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace special:magic
        ;;
    esac
  '';

  swayncLog = pkgs.writeShellScript "swaync-log" ''
    mkdir -p "$HOME/.local/share/swaync"
    printf '%s [%s] %s: %s\n' \
      "$(date -Iseconds)" \
      "$SWAYNC_APP_NAME" \
      "$SWAYNC_SUMMARY" \
      "$SWAYNC_BODY" \
      >> "$HOME/.local/share/swaync/notifications.log"
  '';

  # Screenshot wrapper around hyprshot. Saves under $XDG_SCREENSHOTS_DIR
  # with a GNOME-style filename (hyprshot itself ignores
  # XDG_SCREENSHOTS_DIR and hardcodes a `_hyprshot` suffix), then claims
  # the clipboard with a file:// URI on text/uri-list — the MIME that
  # Firefox / Zulip / Telegram / image editors / file managers all
  # accept and render as the image on paste. wl-copy runs in a transient
  # user-service unit so it survives Hyprland's exec dispatcher tearing
  # down children. The notification's "Open folder" action selects the
  # file in Nautilus.
  hyprshotCmd = pkgs.writeShellScript "hyprshot-cmd" ''
    dir=''${XDG_SCREENSHOTS_DIR:-$HOME/Pictures/Screenshots}
    filename="Screenshot From $(date +'%Y-%m-%d %H-%M-%S').png"
    fullpath="$dir/$filename"

    ${pkgs.hyprshot}/bin/hyprshot --silent -o "$dir" -f "$filename" "$@"
    [ -f "$fullpath" ] || exit 0

    ${pkgs.systemd}/bin/systemd-run --user --collect --quiet --no-block \
      --setenv=WAYLAND_DISPLAY --setenv=XDG_RUNTIME_DIR \
      ${pkgs.bash}/bin/bash -c \
      'printf "file://%s\r\n" "$1" | ${pkgs.wl-clipboard}/bin/wl-copy --foreground --type text/uri-list' \
      wl-copy "$fullpath"

    (
      action=$(${pkgs.libnotify}/bin/notify-send \
        -a Hyprshot -i "$fullpath" \
        -A open="Open folder" \
        "Screenshot saved" "$fullpath" 2>/dev/null)
      [ "$action" = open ] && exec ${pkgs.nautilus}/bin/nautilus --select "$fullpath"
    ) &
  '';

  # Convert a file:// URI clipboard (GTK4 apps — Loupe, Nautilus — put
  # text/uri-list on Hyprland instead of raw image bytes) to raw image/png
  # so terminal apps that only speak image/* MIME types can paste it.
  # Runs on the host so it has full filesystem access regardless of sandbox
  # boundaries. magick handles any format ImageMagick supports.
  clipboardToImage = pkgs.writeShellScript "clipboard-to-image" ''
    uri=$(${pkgs.wl-clipboard}/bin/wl-paste --type text/uri-list 2>/dev/null \
      | head -1 | tr -d '\r\n') || exit 0
    case "$uri" in file://*) ;; *) exit 0 ;; esac
    path=$(${pkgs.python3}/bin/python3 -c \
      "import sys,urllib.parse; print(urllib.parse.unquote(sys.argv[1][7:]))" \
      "$uri" 2>/dev/null) || exit 0
    [ -f "$path" ] || exit 0
    if ${pkgs.imagemagick}/bin/magick "$path" png:- \
        | ${pkgs.wl-clipboard}/bin/wl-copy --type image/png; then
      ${pkgs.libnotify}/bin/notify-send -t 2000 "Clipboard" "Image ready to paste"
    else
      ${pkgs.libnotify}/bin/notify-send -t 2000 "Clipboard" "Not a supported image format"
    fi
  '';

in
{
  imports = [
    ./gui.nix
  ];

  # Point ssh at gnome-keyring's SSH agent socket. gnome-session does this
  # for the GNOME session via dbus-update-activation-environment; under
  # Hyprland nothing exports it, so ssh would otherwise fall back to no
  # agent and re-prompt for the key passphrase every invocation. Path is
  # the daemon's canonical socket (see services.gnome-keyring below).
  home.sessionVariables.SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/keyring/ssh";

  home.packages = with pkgs; [
    # File manager + GNOME Settings panel. Both work standalone outside a
    # GNOME session; control-center panels backed by session-agnostic
    # daemons (NetworkManager, PipeWire, CUPS) keep working, panels that
    # need GNOME Shell (Online Accounts, Background) become no-ops.
    nautilus
    # gnome-control-center refuses to start unless XDG_CURRENT_DESKTOP
    # contains GNOME or Unity; wrap the binary so both terminal invocations
    # and fuzzel launches (via the .desktop file's `Exec=gnome-control-center`)
    # get the override transparently.
    (symlinkJoin {
      name = "gnome-control-center";
      paths = [ gnome-control-center ];
      nativeBuildInputs = [ makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/gnome-control-center \
          --set XDG_CURRENT_DESKTOP GNOME
      '';
    })

    # Wayland Qt support — wiki "Must-have" page recommends installing
    # qt5-wayland and qt6-wayland so Qt apps and the xdg-desktop-portal
    # share-picker render natively.
    qt5.qtwayland
    qt6.qtwayland

    # cliphist's home-manager service pulls in cliphist + wl-clipboard for
    # its own watchers. Tmux yank (base.nix) also pipes through wl-copy
    # outside that service chain, but wl-clipboard is shared via gui.nix.

    # Screenshots — hyprshot wraps grim+slurp with Hyprland IPC awareness
    # (current monitor / focused window selection). Clipboard + notification
    # are handled by the `hyprshotCmd` wrapper in the let block above; grim
    # and slurp stay listed because hyprshot shells out to them, and
    # grimblast is kept as a wlroots-generic fallback.
    grim
    slurp
    grimblast
    hyprshot

    libnotify

    # hypridle + hyprlock + hyprpaper + hyprsunset are configured as
    # services below; their home-manager modules pull in the binaries.

    # Wayland-native colour picker — invoke with the bind below or directly.
    # `-a` auto-copies the picked colour to the clipboard via wl-clipboard
    # (provided by gui.nix); `-f hex` formats as #RRGGBB.
    hyprpicker

    # Media + brightness keys referenced by the example XF86 binds
    brightnessctl
    playerctl
    pavucontrol

    # Host escape-hatch terminal — Ghostty itself runs inside the dev
    # microvm, so kitty is the only host-native terminal. Bound to
    # Super+Shift+Q below; the Hyprland default Super+Q binding still
    # spawns it too if our config fails to load.
    kitty

    # Graphical power menu — Lock / Logout / Suspend / Hibernate / Reboot /
    # Shutdown buttons. Invoked from the Waybar custom/power pill below.
    wlogout

    # Quickshell — QML-based Wayland shell framework. Renders the
    # volume OSD popup; see xdg.configFile."quickshell/osd/shell.qml"
    # below. Started via `exec-once = qs -c osd` in the Hyprland
    # settings.
    pkgs-unstable.quickshell
  ];

  # D-Bus VSOCK proxy for forwarding the VM's desktop notifications to the
  # host — deferred. Re-enable alongside vm.nix's host-dbus-relay when you
  # want notification support.
  /*
    systemd.user.services.dbus-vm-proxy = {
      Unit = {
        Description = "Filtered D-Bus proxy for dev microvm";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = ''
          ${pkgs.xdg-dbus-proxy}/bin/xdg-dbus-proxy \
            unix:path=%t/bus \
            %t/dbus-vm-proxy \
            --filter \
            --talk=org.freedesktop.Notifications
        '';
        Restart = "on-failure";
        RestartSec = 2;
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };

    systemd.user.services.dbus-vm-vsock = {
      Unit = {
        Description = "VSOCK relay for filtered D-Bus proxy";
        After = [ "dbus-vm-proxy.service" ];
        Requires = [ "dbus-vm-proxy.service" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "${pkgs.socat}/bin/socat VSOCK-LISTEN:9999,fork,reuseaddr UNIX-CONNECT:%t/dbus-vm-proxy";
        Restart = "on-failure";
        RestartSec = 2;
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  */

  # Volume OSD as a Quickshell config. Subscribes to PipeWire sink +
  # source state so the popup shows on every volume/mute change —
  # keypress, waybar scroll, AVRCP headphone button, pavucontrol, any
  # app's slider — not just on actions we explicitly bind. Started by
  # its own `qs -c osd` exec-once below.
  xdg.configFile."quickshell/osd/shell.qml".text = ''
    //@ pragma UseQApplication

    import QtQuick
    import QtQuick.Layouts
    import Quickshell
    import Quickshell.Wayland
    import Quickshell.Hyprland
    import Quickshell.Services.Pipewire

    ShellRoot {
        id: root

        property bool osdVisible: false
        property real progress: 0
        property bool muted: false
        property bool isMicChange: false
        // Suppress the spurious volumeChanged signal Pipewire fires
        // when the daemon first connects and reads initial state.
        property bool ready: false

        // Keep the audio sub-objects alive so their property change
        // signals actually reach the Connections below.
        PwObjectTracker {
            objects: [Pipewire.defaultAudioSink, Pipewire.defaultAudioSource]
        }

        Connections {
            target: Pipewire.defaultAudioSink?.audio ?? null
            function refresh() {
                if (!root.ready) return;
                root.isMicChange = false;
                root.muted = Pipewire.defaultAudioSink?.audio?.muted ?? false;
                root.progress = Pipewire.defaultAudioSink?.audio?.volume ?? 0;
                root.osdVisible = true;
                hideTimer.restart();
            }
            function onVolumeChanged() { refresh(); }
            function onMutedChanged() { refresh(); }
        }

        Connections {
            target: Pipewire.defaultAudioSource?.audio ?? null
            function refresh() {
                if (!root.ready) return;
                root.isMicChange = true;
                root.muted = Pipewire.defaultAudioSource?.audio?.muted ?? false;
                root.progress = Pipewire.defaultAudioSource?.audio?.volume ?? 0;
                root.osdVisible = true;
                hideTimer.restart();
            }
            function onVolumeChanged() { refresh(); }
            function onMutedChanged() { refresh(); }
        }

        Timer {
            id: readyTimer
            interval: 1000
            running: true
            onTriggered: root.ready = true
        }

        Timer {
            id: hideTimer
            interval: 1500
            onTriggered: root.osdVisible = false
        }

        LazyLoader {
            active: root.osdVisible

            PanelWindow {
                screen: Quickshell.screens.find(s => s.name === Hyprland.focusedMonitor?.name) ?? null

                WlrLayershell.layer: WlrLayer.Overlay
                WlrLayershell.exclusionMode: ExclusionMode.Ignore
                WlrLayershell.namespace: "quickshell:osd"

                color: "transparent"
                implicitWidth: 360
                implicitHeight: 64
                margins.top: 80
                anchors.top: true

                // Catppuccin Mocha — base bg, surface0 track, blue
                // accent, overlay0 for the muted state. Matches the
                // waybar pulseaudio pill's palette in style.css above.
                Rectangle {
                    anchors.fill: parent
                    anchors.margins: 8
                    color: "#1e1e2e"
                    radius: 12
                    border.width: 1
                    border.color: "#45475a"

                    RowLayout {
                        anchors.fill: parent
                        anchors.leftMargin: 16
                        anchors.rightMargin: 16
                        spacing: 14

                        Text {
                            Layout.preferredWidth: 22
                            Layout.alignment: Qt.AlignVCenter
                            text: root.muted ? ""
                                : root.isMicChange ? ""
                                : root.progress < 0.34 ? ""
                                : ""
                            font.family: "FiraCode Nerd Font Mono"
                            font.pixelSize: 18
                            color: root.muted ? "#6c7086" : "#cdd6f4"
                            horizontalAlignment: Text.AlignHCenter
                        }

                        Rectangle {
                            Layout.fillWidth: true
                            Layout.preferredHeight: 6
                            Layout.alignment: Qt.AlignVCenter
                            radius: 3
                            color: "#313244"

                            Rectangle {
                                width: parent.width * Math.min(1, Math.max(0, root.progress))
                                height: parent.height
                                radius: parent.radius
                                color: root.muted ? "#6c7086" : "#89b4fa"
                                Behavior on width {
                                    NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
                                }
                            }
                        }

                        Text {
                            Layout.preferredWidth: 44
                            Layout.alignment: Qt.AlignVCenter
                            text: Math.round(root.progress * 100) + "%"
                            font.family: "FiraCode Nerd Font Mono"
                            font.pixelSize: 13
                            color: root.muted ? "#6c7086" : "#cdd6f4"
                            horizontalAlignment: Text.AlignRight
                        }
                    }
                }
            }
        }
    }
  '';

  # Hyprland-specific addition over gui.nix's image MIME defaults — under
  # GNOME, Nautilus is already the default file manager; spelling it out
  # here ensures `xdg-open .` lands on Nautilus under Hyprland too.
  xdg.mimeApps.defaultApplications."inode/directory" = [ "org.gnome.Nautilus.desktop" ];

  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 24;
  };

  ###########################################################################
  # Hyprland compositor. The `settings` block started as a translation of
  # the upstream example config from pkgs.hyprland (share/hypr/hyprland.conf
  # at the version pinned by nixpkgs), extended with Vim-style hjkl focus
  # and per-monitor workspace conventions; see the comment block above the
  # workspace binds for the modifier convention.
  ###########################################################################
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    # NixOS module owns the Hyprland and XDPH packages; setting these to null
    # tells home-manager to use the system ones so versions can't diverge.
    # (https://wiki.hypr.land/Nix/Hyprland-on-Home-Manager/#using-the-home-manager-module-with-nixos)
    package = null;
    portalPackage = null;
    # `systemd.variables = ["--all"]` exports the user's environment into
    # systemd so hypridle/hyprlock/waybar can resolve $PATH etc. (Hyprland
    # wiki: "Programs don't work in systemd services").
    systemd = {
      enable = true;
      variables = [ "--all" ];
    };

    settings = {
      "$mod" = "SUPER";
      # Super+T opens ghostty already ssh'd into the dev microvm (via the
      # ssh-dev-vm wrapper, which forwards GH_TOKEN/NIX_CONFIG).
      # Super+Shift+T (below) opens a plain host ghostty.
      "$terminal" = "ghostty -e ssh-dev-vm";
      "$fileManager" = "nautilus";
      "$menu" = "fuzzel";

      monitor = [
        # 34" ultrawide (Viewteck GNV34DBE) anchors the layout at the origin.
        "DP-2, 3440x1440@144, 0x0, 1"
        # HP VH240a centered horizontally above the ultrawide: x = (3440 - 1920) / 2.
        # Y is negative because Hyprland uses inverse-Y (negative = up).
        "HDMI-A-1, 1920x1080@60, 760x-1080, 1"
      ];

      # Workspaces pinned per monitor so the Super+N synced pair-switch
      # binds (further down) are deterministic. `persistent:true` keeps
      # them around even when empty; `default:true` is the workspace each
      # monitor opens on at session start.
      workspace = [
        "1, monitor:DP-2, default:true, persistent:true"
        "2, monitor:DP-2, persistent:true"
        "3, monitor:DP-2, persistent:true"
        "4, monitor:DP-2, persistent:true"
        "5, monitor:DP-2, persistent:true"
        "6, monitor:HDMI-A-1, default:true, persistent:true"
        "7, monitor:HDMI-A-1, persistent:true"
        "8, monitor:HDMI-A-1, persistent:true"
        "9, monitor:HDMI-A-1, persistent:true"
        "10, monitor:HDMI-A-1, persistent:true"
      ];

      env = [
        "XCURSOR_THEME,Bibata-Modern-Classic"
        "XCURSOR_SIZE,24"
        "HYPRCURSOR_SIZE,24"
        # Canonical screenshots dir — grimblast and most XDG-aware tools
        # honour this. Path is expanded at eval time (Hyprland's `env`
        # values are literal strings, not shell-expanded at use site).
        "XDG_SCREENSHOTS_DIR,${config.home.homeDirectory}/Pictures/Screenshots"
        # GDM doesn't propagate the session env to Hyprland, so GTK apps
        # launched from keybind `exec` (e.g. wlogout via Super+Esc) miss
        # the librsvg loader and render SVGs as broken-image placeholders.
        # Services started under systemd-user pick this up separately.
        "GDK_PIXBUF_MODULE_FILE,${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache"
      ];

      # All session daemons (cliphist, nm-applet, blueman-applet, swaync,
      # hyprpolkitagent) are managed by home-manager systemd user services
      # below, bound to wayland-session.target — no exec-once needed.
      # Quickshell is the exception: not wired as a systemd user service,
      # so we exec-once the volume OSD here. Its QML lives in
      # xdg.configFile."quickshell/osd/shell.qml".
      exec-once = [
        "${pkgs-unstable.quickshell}/bin/qs -c osd"
      ];

      # Only customisations vs upstream defaults are set here. Anything
      # not listed inherits Hyprland's built-in default
      # (src/config/values/ConfigValues.cpp).
      general = {
        border_size = 2; # default: 1
        "col.active_border" = "rgba(89b4faee) rgba(cba6f7ee) 45deg"; # blue → mauve
        "col.inactive_border" = "rgba(313244aa)"; # surface0
      };

      decoration = {
        rounding = 10; # default: 0
        blur.size = 3; # default: 8
        # Slight dim on unfocused windows so the focused one reads first
        # at a glance — important on the ultrawide where 3+ tiles are
        # visible at once. Fullscreen always renders at full opacity.
        active_opacity = 1.0; # default: 1.0
        inactive_opacity = 0.85; # default: 1.0
        fullscreen_opacity = 1.0; # default: 1.0
      };

      dwindle = {
        preserve_split = true; # default: false
      };

      misc = {
        # Apps raise their window on notification click via the EWMH
        # _NET_ACTIVE_WINDOW request; Hyprland ignores those by default.
        focus_on_activate = true;
      };

      # Example binds from the upstream config. See
      # https://wiki.hypr.land/Configuring/Basics/Binds/ for keyword reference.
      bind = [
        "$mod ALT, Q, exec, loginctl lock-session"
        "$mod, Escape, exec, pidof wlogout || ${pkgs.wlogout}/bin/wlogout -L 1200 -R 1200 -T 350 -B 350"
        "$mod, X, killactive,"
        "$mod, M, exit,"
        "$mod, E, exec, $fileManager"
        "$mod, V, togglefloating,"
        "$mod, F, fullscreen, 1"
        "$mod, BackSpace, exec, $menu"
        # Pseudo moved off P to free it for the screenshot scheme below.
        "$mod, I, pseudo,"
        # Colour picker — C for "colour"; moved off P for the same reason.
        "$mod, C, exec, hyprpicker -a -f hex"
        "$mod SHIFT, D, exec, darkman toggle"
        "$mod, T, exec, $terminal"
        # Plain host ghostty — not tied to the VM.
        "$mod SHIFT, T, exec, ghostty"

        # Screenshots via hyprshot — every mode saves to
        # ~/Pictures/Screenshots AND copies the image to the clipboard;
        # swaync surfaces a thumbnail preview. Super+P: region drag-
        # select. +Shift: focused window. +Alt: whole focused monitor.
        "$mod, P, exec, ${hyprshotCmd} -m region"
        "$mod SHIFT, P, exec, ${hyprshotCmd} -m window"
        "$mod ALT, P, exec, ${hyprshotCmd} -m output"

        # Convert a file:// URI clipboard to raw image/png. GTK4 apps
        # (Loupe, Nautilus) copy images as text/uri-list on Hyprland;
        # press this after copying from an image viewer, then paste
        # normally in Claude Code or any app that needs raw image bytes.
        "$mod CTRL, P, exec, ${clipboardToImage}"

        "$mod, H, movefocus, l"
        "$mod, J, movefocus, d"
        "$mod, K, movefocus, u"
        "$mod, L, movefocus, r"

        # Move the focused window in a direction (swap within the layout).
        "$mod ALT, H, movewindow, l"
        "$mod ALT, J, movewindow, d"
        "$mod ALT, K, movewindow, u"
        "$mod ALT, L, movewindow, r"

        # Toggle the dwindle split orientation (horizontal/vertical) of the
        # focused container.
        "$mod ALT, T, togglesplit,"

        # Jump to a screen edge.
        # J/K cross the vertical monitor boundary (K → HDMI-A-1, J → DP-2).
        # H/L focus the left/rightmost window on the *current* monitor
        # via the helper script — workspace filtering picks the right
        # monitor's clients automatically.
        "$mod SHIFT, H, exec, ${focusEdgeWindow} l"
        "$mod SHIFT, J, focusmonitor, d"
        "$mod SHIFT, K, focusmonitor, u"
        "$mod SHIFT, L, exec, ${focusEdgeWindow} r"

        # Modifier convention:
        #   ALT = movetoworkspace (send focused window instead of switching)
        # Numbers are absolute: 1-5 = DP-2 (bottom monitor) workspaces 1-5,
        # 6-0 = HDMI-A-1 (top monitor) workspaces 6-10. Arrows are
        # direction-based so they always act on the *current* monitor
        # (m+1/m-1 wraps within the active monitor's workspace range).

        # --- Switch workspace ---
        "$mod, left, workspace, m-1"
        "$mod, right, workspace, m+1"

        "$mod, 1, workspace, 1"
        "$mod, 2, workspace, 2"
        "$mod, 3, workspace, 3"
        "$mod, 4, workspace, 4"
        "$mod, 5, workspace, 5"
        "$mod, 6, workspace, 6"
        "$mod, 7, workspace, 7"
        "$mod, 8, workspace, 8"
        "$mod, 9, workspace, 9"
        "$mod, 0, workspace, 10"

        # --- Move focused window to workspace (Alt prefix) ---
        "$mod ALT, left, movetoworkspace, m-1"
        "$mod ALT, right, movetoworkspace, m+1"

        # Move focused window to ws N on the *current* monitor (script
        # adjusts target by +5 when cursor is on HDMI-A-1).
        "$mod ALT, 1, exec, ${moveToWsRelative} 1"
        "$mod ALT, 2, exec, ${moveToWsRelative} 2"
        "$mod ALT, 3, exec, ${moveToWsRelative} 3"
        "$mod ALT, 4, exec, ${moveToWsRelative} 4"
        "$mod ALT, 5, exec, ${moveToWsRelative} 5"
        "$mod ALT, 6, movetoworkspace, 6"
        "$mod ALT, 7, movetoworkspace, 7"
        "$mod ALT, 8, movetoworkspace, 8"
        "$mod ALT, 9, movetoworkspace, 9"
        "$mod ALT, 0, movetoworkspace, 10"

        "$mod, S, togglespecialworkspace, magic"
        # Toggle the focused window in/out of the magic scratchpad,
        # following it to the destination workspace either way.
        "$mod ALT, S, exec, ${scratchToggle}"

        "$mod, mouse_down, workspace, e+1"
        "$mod, mouse_up, workspace, e-1"

      ];

      # Mouse drag move/resize (upstream example)
      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];

      binde = [
        "$mod CTRL, h, resizeactive, -30 0"
        "$mod CTRL, j, resizeactive, 0 30"
        "$mod CTRL, k, resizeactive, 0 -30"
        "$mod CTRL, l, resizeactive, 30 0"
      ];

      # Volume goes through wpctl directly so the resulting PipeWire
      # state change is the OSD trigger — the Quickshell `osd` config
      # below subscribes to PipeWire and shows the popup on every
      # change (waybar scroll, AVRCP headphone buttons, pavucontrol,
      # any app's slider) rather than only on these keypresses.
      # Brightness still routes through swayosd-client because there's
      # no PipeWire-equivalent event source for backlight changes.
      bindel = [
        ",XF86AudioRaiseVolume, exec, ${pkgs.wireplumber}/bin/wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+"
        ",XF86AudioLowerVolume, exec, ${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ",XF86AudioMute, exec, ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ",XF86AudioMicMute, exec, ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
        ",XF86MonBrightnessUp, exec, swayosd-client --brightness raise"
        ",XF86MonBrightnessDown, exec, swayosd-client --brightness lower"
      ];

      bindl = [
        ", XF86AudioNext, exec, playerctl next"
        ", XF86AudioPause, exec, playerctl play-pause"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioPrev, exec, playerctl previous"
      ];

      windowrule = [
        # Ignore maximize requests from apps — Hyprland's tiler handles layout.
        "suppress_event maximize, match:class .*"
        # Fix some dragging issues with XWayland
        "no_focus true, match:class ^$, match:title ^$, match:xwayland 1, match:float 1, match:fullscreen 0, match:pin 0"
      ];
    };

  };

  # Two Waybar instances, each pinned to one output (omitting `output`
  # spawns on every monitor and races across rebuilds). `primaryBar` on
  # the ultrawide covers the GNOME top-right popup equivalent (workspaces,
  # tray, idle inhibitor, audio, network, clock); nm-applet +
  # blueman-applet surface in `tray`. `secondaryBar` on HDMI-A-1 is
  # minimal — just workspaces + focused window title — so the upper
  # monitor's active workspace is visible at a glance.
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    # Catppuccin Mocha palette + styling adapted from rubyowo's reference
    # dotfiles (linked from the catppuccin/waybar README as the preview
    # image source — github.com/rubyowo/dotfiles, commit f925cf8).
    # Aesthetic: transparent bar with floating @surface0 pill modules,
    # rounded outer corners on the leftmost (cpu) and rightmost (clock)
    # of the right group, per-module accent colours, 15pt FiraCode.
    style = ''
      @define-color rosewater #f5e0dc;
      @define-color flamingo  #f2cdcd;
      @define-color pink      #f5c2e7;
      @define-color mauve     #cba6f7;
      @define-color red       #f38ba8;
      @define-color maroon    #eba0ac;
      @define-color peach     #fab387;
      @define-color yellow    #f9e2af;
      @define-color green     #a6e3a1;
      @define-color teal      #94e2d5;
      @define-color sky       #89dceb;
      @define-color sapphire  #74c7ec;
      @define-color blue      #89b4fa;
      @define-color lavender  #b4befe;
      @define-color text      #cdd6f4;
      @define-color subtext1  #bac2de;
      @define-color subtext0  #a6adc8;
      @define-color overlay2  #9399b2;
      @define-color overlay1  #7f849c;
      @define-color overlay0  #6c7086;
      @define-color surface2  #585b70;
      @define-color surface1  #45475a;
      @define-color surface0  #313244;
      @define-color base      #1e1e2e;
      @define-color mantle    #181825;
      @define-color crust     #11111b;

      * {
        font-family: "FiraCode Nerd Font", sans-serif;
        font-size: 15px;
        font-weight: bold;
        min-height: 0;
      }

      window#waybar {
        background: transparent;
        color: @text;
        margin: 5px 5px;
      }

      /* Left: workspaces pill */
      #workspaces {
        border-radius: 1rem;
        margin: 5px;
        background-color: @surface0;
        margin-left: 1rem;
      }
      #workspaces button {
        color: @lavender;
        border-radius: 1rem;
        padding: 0.2rem 0.5rem;
      }
      #workspaces button.active {
        background-color: @sky;
        color: @base;
        border-radius: 1rem;
        font-weight: bold;
      }
      #workspaces button.urgent {
        color: @red;
      }
      #workspaces button:hover {
        color: @sapphire;
        border-radius: 1rem;
      }

      /* Center: focused window title, matching @surface0 pill so the
         text stays legible over light wallpapers. */
      #window {
        color: @text;
        background-color: @surface0;
        border-radius: 1rem;
        padding: 0.3rem 0.75rem;
        margin: 5px 0;
      }

      /* Center: submap indicator. Hidden by Waybar when no submap is
         active; flips to a filled peach pill the moment one is entered
         (e.g. resize mode via Super+R). Dark text + bright background so
         it's impossible to miss against the transparent bar. */
      #submap {
        background-color: @peach;
        color: @base;
        border-radius: 1rem;
        padding: 0.3rem 0.75rem;
        margin: 5px 0.5rem;
      }

      /* Right side: four independent pills.
         Group 1 (system metrics): cpu - memory - temperature - disk - power-profiles-daemon
         Group 2 (tray): standalone
         Group 3 (connectivity/controls): idle_inhibitor - pulseaudio - network - custom-notification
         Group 4 (clock): standalone */
      #cpu,
      #memory,
      #temperature,
      #temperature.gpu,
      #disk,
      #power-profiles-daemon,
      #tray,
      #custom-darkman,
      #custom-screenshot,
      #idle_inhibitor,
      #pulseaudio,
      #network,
      #custom-notification,
      #clock,
      #custom-power {
        background-color: @surface0;
        padding: 0.5rem 0.75rem;
        margin: 5px 0;
      }

      /* Group 1: system metrics */
      #cpu {
        color: @peach;
        border-radius: 1rem 0 0 1rem;
        margin-left: 1rem;
      }
      #memory {
        color: @yellow;
      }
      #temperature {
        color: @maroon;
      }
      /* GPU temp pill — sapphire to distinguish from the maroon CPU pill
         sitting just to its left. */
      #temperature.gpu {
        color: @sapphire;
      }
      /* Above critical-threshold: pill flips to a filled red background
         with dark text so it's hard to miss. Applies to both CPU and
         GPU pills via the shared #temperature ID. */
      #temperature.critical {
        background-color: @red;
        color: @base;
      }
      #disk {
        color: @teal;
      }
      #power-profiles-daemon {
        color: @mauve;
        border-radius: 0 1rem 1rem 0;
        /* Tighter left padding so the icon sits closer to temperature;
           extra right padding so content doesn't crowd the rounded edge. */
        padding-left: 0.25rem;
        padding-right: 1rem;
        margin-right: 0.5rem;
      }
      #power-profiles-daemon.performance {
        color: @red;
      }
      #power-profiles-daemon.power-saver {
        color: @green;
      }

      /* Group 2: tray (standalone pill) */
      #tray {
        color: @text;
        border-radius: 1rem;
        margin-right: 0.5rem;
      }

      /* darkman toggle (standalone pill between tray and connectivity).
         Glyph colour follows mode via the `class` field on the JSON
         output — sun-yellow when light, lavender when dark. */
      #custom-darkman {
        border-radius: 1rem;
        margin-right: 0.5rem;
      }
      #custom-darkman.light {
        color: @yellow;
      }
      #custom-darkman.dark {
        color: @lavender;
      }

      /* Screenshot trigger (standalone pill, sapphire-blue camera).
         Padding asymmetric to compensate for the U+F030 camera glyph's
         internal whitespace within its character cell — same trick as
         #custom-power below. Shared rule is 0.75rem horizontal; nudge
         by 0.125rem so the glyph sits visually centered. */
      #custom-screenshot {
        color: @sapphire;
        border-radius: 1rem;
        padding-left: 0.625rem;
        padding-right: 0.875rem;
        margin-right: 0.5rem;
      }

      /* Group 3: connectivity / controls */
      #idle_inhibitor {
        color: @lavender;
        border-radius: 1rem 0 0 1rem;
      }
      #idle_inhibitor.activated {
        color: @yellow;
      }
      #pulseaudio {
        color: @maroon;
      }
      #pulseaudio.muted {
        color: @overlay0;
      }
      #network {
        color: @teal;
      }
      #network.disconnected {
        color: @overlay0;
      }
      #custom-notification {
        color: @pink;
        border-radius: 0 1rem 1rem 0;
        /* Extra right-padding so the bell glyph doesn't crowd the rounded edge. */
        padding-right: 1rem;
        margin-right: 0.5rem;
      }

      /* Group 4: clock (standalone pill) */
      #clock {
        color: @text;
        border-radius: 1rem;
      }

      /* Group 5: power button (standalone pill) — click opens wlogout.
         Padding is slightly asymmetric to compensate for the U+F011 power
         glyph's own internal whitespace within its character cell.
         Shared rule is 0.75rem horizontal; we nudge by 0.125rem. */
      #custom-power {
        color: @red;
        border-radius: 1rem;
        padding-left: 0.625rem;
        padding-right: 0.875rem;
        margin-left: 0.5rem;
        margin-right: 1rem;
      }

      tooltip {
        background: @surface0;
        color: @text;
        border: 1px solid @overlay0;
      }
    '';
    settings.primaryBar = {
      output = [ "DP-2" ];
      layer = "top";
      position = "top";
      # 0 removes the default 4px inter-module gap so each pill group
      # joins seamlessly; the gaps *between* groups come from the
      # margin-right values on the last module of each group in style.css.
      spacing = 0;
      modules-left = [ "hyprland/workspaces" ];
      modules-center = [
        "hyprland/submap"
        "hyprland/window"
      ];
      modules-right = [
        "cpu"
        "memory"
        "temperature"
        "temperature#gpu"
        "disk"
        "power-profiles-daemon"
        "tray"
        "custom/darkman"
        "custom/screenshot"
        "idle_inhibitor"
        "pulseaudio"
        "network"
        "custom/notification"
        "clock"
        "custom/power"
      ];
      "hyprland/submap" = {
        # Hyprland emits an IPC `submap` event on enter/exit; Waybar's
        # module hides itself when the submap is empty (default state) and
        # shows this format string while one is active. Adds a CSS class
        # matching the submap name (e.g. `#submap.resize`) for per-mode
        # styling if ever needed.
        format = "󰩨 {}"; # nf-md-resize
        tooltip = false;
      };
      "hyprland/window" = {
        # Prefix the title with the app class so visually-identical
        # terminals (ghostty + kitty share a theme) are distinguishable.
        # `rewrite` keys are full-match regexes; the `(.*)` tail recaptures
        # the title. Ghostty's Wayland class is the reverse-DNS app_id.
        separate-outputs = true;
        format = "{class}  {title}";
        rewrite = {
          "com.mitchellh.ghostty  (.*)" = "  ghostty  $1"; # nf-fa-terminal
          "kitty  (.*)" = "  kitty  $1";
        };
      };
      clock = {
        # Pango span (typecraft-style) colours just the calendar glyph
        # @pink — the time itself inherits the module's @text colour.
        format = "<span foreground='#f5c2e7'></span>  {:%a %b %d  %I:%M %p}";
        tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
      };
      # Glyphs below are Nerd Font codepoints from Waybar's upstream
      # config.jsonc. They render blank in plain editors but resolve to
      # icons via the FiraCode Nerd Font installed in gui.nix.
      cpu.format = "{usage}% "; # nf-fa-microchip
      memory.format = "{}% "; # nf-fa-memory
      cpu.interval = 1;
      memory.interval = 1;
      disk = {
        format = "{free} 󰋊"; # nf-md-harddisk
        tooltip-format = "{used} used / {total} total on {path}";
      };
      tray.spacing = 10;
      power-profiles-daemon = {
        format = "{icon}";
        tooltip-format = "Power profile: {profile}\nDriver: {driver}";
        format-icons = {
          default = ""; # nf-fa-bolt
          performance = ""; # nf-fa-bolt
          balanced = ""; # nf-fa-balance-scale
          power-saver = ""; # nf-fa-leaf
        };
      };
      pulseaudio = {
        format = "{volume}% {icon}";
        format-muted = "";
        format-icons.default = [
          ""
          ""
          ""
        ];
        format-bluetooth = "{volume}% {icon}"; # nf-fa-bluetooth
        format-bluetooth-muted = "{icon}";
        # Scroll/click adjust PipeWire directly via wpctl — the
        # Quickshell `osd` config picks up the resulting state change
        # and renders the popup, the same way the XF86Audio binds and
        # external sources (BT headphones, pavucontrol) flow through.
        # Right-click still opens pavucontrol for routing.
        on-scroll-up = "${pkgs.wireplumber}/bin/wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+";
        on-scroll-down = "${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
        on-click = "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
        on-click-right = "pavucontrol";
      };
      temperature = {
        # Pin to k10temp's Tctl on the AMD CPU. Without this, waybar's
        # auto-pick lands on the acpitz chassis sensor which sits at room
        # temperature (~17°C) regardless of CPU load. The PCI path is the
        # stable host bridge for k10temp on Ryzen platforms (function 18.3
        # under the root domain); hwmonN inside it can be renumbered, but
        # there's only one hwmon entry under this directory so waybar
        # picks it unambiguously.
        hwmon-path-abs = "/sys/devices/pci0000:00/0000:00:18.3/hwmon";
        input-filename = "temp1_input";
        interval = 5;
        critical-threshold = 80;
        format = "{temperatureC}°C 󰔏"; # nf-md-thermometer
      };
      # dGPU edge temp from amdgpu. The PCI path is for the discrete card
      # (multiple bridges deep); the iGPU's amdgpu sibling sits closer to
      # the root and is ignored here. critical bumped to 90°C because AMD
      # dGPUs sustain higher idle/load temps than the CPU.
      "temperature#gpu" = {
        hwmon-path-abs = "/sys/devices/pci0000:00/0000:00:01.1/0000:01:00.0/0000:02:00.0/0000:03:00.0/hwmon";
        input-filename = "temp1_input";
        interval = 5;
        critical-threshold = 90;
        format = "{temperatureC}°C 󰢮"; # nf-md-vga
      };
      network = {
        interval = 5;
        # Right-pad each value so the pill width is constant — without
        # padding, the whole right side of the bar shifts left/right
        # every second as digits roll over (e.g. 9KB → 10KB → 100KB).
        # Waybar auto-bumps the unit at 1000, so bandwidth maxes at
        # "999XB" (5 chars) at any magnitude; signal is 0–100 (3 chars).
        format-wifi = " {bandwidthDownBytes:>5}  {bandwidthUpBytes:>5}   {signalStrength:>3}% "; # nf-fa-arrow-down, nf-fa-arrow-up, nf-fa-wifi
        format-ethernet = " {bandwidthDownBytes:>5}  {bandwidthUpBytes:>5}  󰌗 "; # nf-fa-arrow-down, nf-fa-arrow-up, nf-md-lan
        format-disconnected = "Disconnected  "; # nf-fa-times
        tooltip-format = "{ifname}: {ipaddr}";
      };
      "custom/notification" = {
        tooltip = false;
        format = "{icon}";
        format-icons = {
          notification = "󰂚"; # nf-md-bell_badge
          none = "󰂜"; # nf-md-bell_off
          dnd-notification = "󰂛"; # nf-md-bell_cancel
          dnd-none = "󰂛";
        };
        return-type = "json";
        exec-if = "which swaync-client";
        exec = "swaync-client -swb";
        on-click = "swaync-client -t -sw";
        on-click-right = "swaync-client -d -sw";
        escape = true;
      };
      # darkman state pill — `class` is set to "light"/"dark" so the CSS
      # below can colour the glyph differently per mode. `signal = 8` makes
      # Waybar re-run `exec` on SIGRTMIN+8, which darkman's light/dark
      # mode-scripts (services.darkman below) raise immediately on
      # transition — instant icon update without a busy poll. `interval`
      # is a 60s fallback in case the signal is dropped.
      "custom/darkman" = {
        return-type = "json";
        format = "{}";
        exec = "${pkgs.writeShellScript "waybar-darkman" ''
          # darkman 2.x prints `LIGHT` / `DARK` (uppercase) followed by a
          # newline; normalise both case and whitespace so the case match
          # is stable across versions.
          state=$(${pkgs.darkman}/bin/darkman get 2>/dev/null \
            | tr '[:upper:]' '[:lower:]' \
            | tr -d '[:space:]')
          # Glyphs declared via $'\uXXXX' (bash ANSI-C quoting) instead
          # of literal UTF-8 bytes — keeps the Nix source ASCII-safe and
          # immune to editor-roundtrip stripping of non-ASCII characters.
          # F185 = nf-fa-sun_o, F186 = nf-fa-moon_o.
          case "$state" in
            light) icon=$'' ;;
            dark)  icon=$'' ;;
            *)     state="unknown"; icon="?" ;;
          esac
          # Use a heredoc-free printf with the long-form em-dash escape
          # for the same reason.
          printf '{"text":"%s","class":"%s","alt":"%s","tooltip":"%s mode — click to toggle"}\n' \
            "$icon" "$state" "$state" "$state"
        ''}";
        on-click = "${pkgs.darkman}/bin/darkman toggle";
        signal = 8;
        interval = 60;
      };
      "custom/power" = {
        tooltip = false;
        format = ""; # nf-fa-power_off (U+F011) — injected as a literal byte below
        # wlogout default stretches buttons to fill the screen — passing per-side
        # margins shrinks the active area. On a 3440px ultrawide, 1200px L/R +
        # 350px T/B centres the 2x3 grid at \~340x370 buttons.
        on-click = "${pkgs.wlogout}/bin/wlogout -L 1200 -R 1200 -T 350 -B 350";
      };
      # Screenshot pill — mirrors the Print/Shift+Print/Ctrl+Print binds.
      # Click: drag-select region. Right-click: focused window.
      # Middle-click: whole focused monitor. All three save to
      # ~/Pictures/Screenshots and copy the image to the clipboard.
      "custom/screenshot" = {
        tooltip = true;
        tooltip-format = "Click: region\nRight-click: window\nMiddle-click: monitor";
        format = ""; # nf-fa-camera (U+F030)
        on-click = "${hyprshotCmd} -m region";
        on-click-right = "${hyprshotCmd} -m window";
        on-click-middle = "${hyprshotCmd} -m output";
      };
      idle_inhibitor = {
        format = "{icon}";
        format-icons = {
          activated = "";
          deactivated = "";
        };
        tooltip-format-activated = "Idle inhibited (presentation mode)";
        tooltip-format-deactivated = "Click to inhibit idle";
      };
    };
    settings.secondaryBar = {
      output = [ "HDMI-A-1" ];
      layer = "top";
      position = "top";
      spacing = 0;
      modules-left = [ "hyprland/workspaces" ];
      modules-center = [
        "hyprland/submap"
        "hyprland/window"
      ];
      modules-right = [ ];
      "hyprland/window" = {
        separate-outputs = true;
        format = "{class}  {title}";
        rewrite = {
          "com.mitchellh.ghostty  (.*)" = "  ghostty  $1"; # nf-fa-terminal
          "kitty  (.*)" = "  kitty  $1";
        };
      };
    };
  };

  # hyprpaper — wallpaper daemon. Per-monitor wallpapers sourced from
  # ~/Pictures (not the Nix store) so they're easy to swap without a
  # rebuild. `splash = false` suppresses the boot splash overlay; each
  # `wallpaper` entry is `MONITOR,PATH` and the corresponding image must
  # appear in `preload` first.
  services.hyprpaper = {
    enable = true;
    settings = {
      splash = false;
      preload = [
        "${config.home.homeDirectory}/Pictures/wallhaven-1.jpg"
        "${config.home.homeDirectory}/Pictures/wallhaven-2.jpg"
      ];
      wallpaper = [
        "DP-2, ${config.home.homeDirectory}/Pictures/wallhaven-1.jpg"
        "HDMI-A-1, ${config.home.homeDirectory}/Pictures/wallhaven-2.jpg"
      ];
    };
  };

  # hypridle — dim → DPMS off (power save) → lock (security threshold).
  # Black screen and lock are deliberately separate: walking away for a
  # minute should not require re-authing. hyprsunset's gamma filter
  # handles the dim step (works on externals without backlight). Waybar's
  # idle_inhibitor module ties into the same wayland-inhibit protocol
  # hypridle honours, so clicking the eye pauses every timer
  # (Caffeine-style presentation mode).
  services.hypridle = {
    enable = true;
    settings = {
      general = {
        after_sleep_cmd = "hyprctl dispatch dpms on";
        before_sleep_cmd = "loginctl lock-session";
        lock_cmd = "pidof hyprlock || hyprlock";
      };
      listener = [
        {
          timeout = 295;
          on-timeout = "hyprctl hyprsunset gamma 50";
          on-resume = "hyprctl hyprsunset gamma 100";
        }
        {
          timeout = 300;
          # Per-output Waybar bars leak surfaces across a wl_output
          # destroy/create cycle, leaving N stacked copies on a monitor
          # after a few DPMS idle/wake rounds. Stopping Waybar before
          # the output is destroyed avoids the bug condition.
          on-timeout = "systemctl --user stop waybar.service && hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on && systemctl --user start waybar.service";
        }
        {
          timeout = 600;
          on-timeout = "loginctl lock-session";
        }
      ];
    };
  };

  # hyprlock — upstream example config (share/hypr/hyprlock.conf at the
  # version pinned by nixpkgs), minus the en/ru layout-switcher label.
  programs.hyprlock = {
    enable = true;
    settings = {
      general = {
        hide_cursor = false;
      };
      animations = {
        enabled = true;
        bezier = "linear, 1, 1, 0, 0";
        animation = [
          "fadeIn, 1, 5, linear"
          "fadeOut, 1, 5, linear"
          "inputFieldDots, 1, 2, linear"
        ];
      };
      background = [
        {
          monitor = "";
          path = "screenshot";
          blur_passes = 3;
        }
      ];
      input-field = [
        {
          monitor = "DP-2";
          size = "20%, 5%";
          outline_thickness = 3;
          inner_color = "rgba(0, 0, 0, 0.0)";
          outer_color = "rgba(89b4faee) rgba(cba6f7ee) 45deg"; # blue → mauve
          check_color = "rgba(a6e3a1ee) rgba(94e2d5ee) 120deg"; # green → teal
          fail_color = "rgba(f38ba8ee) rgba(eba0acee) 40deg"; # red → maroon
          font_color = "rgb(205, 214, 244)"; # text
          fade_on_empty = false;
          rounding = 15;
          font_family = "Monospace";
          placeholder_text = "Input password...";
          fail_text = "$PAMFAIL";
          dots_spacing = 0.3;
          position = "0, -20";
          halign = "center";
          valign = "center";
        }
      ];
      label = [
        {
          monitor = "DP-2";
          text = "$TIME";
          font_size = 90;
          font_family = "Monospace";
          position = "-30, 0";
          halign = "right";
          valign = "top";
        }
        {
          monitor = "DP-2";
          text = ''cmd[update:60000] date +"%A, %d %B %Y"'';
          font_size = 25;
          font_family = "Monospace";
          position = "-30, -150";
          halign = "right";
          valign = "top";
        }
      ];
    };
  };

  # hyprsunset — Night Light equivalent (replaces gnome-control-center's
  # toggle, which is inert under Hyprland). 4000K matches GNOME's default
  # night-light colour; sits in the 3500-4500K "warm but not orange"
  # range. The daemon also handles hypridle's runtime `gamma` IPC calls
  # for the dim-before-DPMS-off step — the active profile's gamma is
  # overridden until the next profile boundary, so the two uses don't
  # fight.
  services.hyprsunset = {
    enable = true;
    settings.profile = [
      {
        time = "06:30";
        identity = true;
      }
      {
        time = "20:00";
        temperature = 4000;
      }
    ];
  };

  # darkman — light/dark mode scheduler. Replicates GNOME's auto colour-
  # scheme behaviour under Hyprland: at dawn it writes
  # `org.gnome.desktop.interface color-scheme = prefer-light` to dconf;
  # at dusk it flips back to `prefer-dark`. xdg-desktop-portal-gtk reads
  # that dconf key and re-broadcasts via the freedesktop `Settings`
  # portal — Ghostty's `dark:.../light:...` theme split, auto-dark-mode.nvim
  # (apps/nvim.nix), and Firefox's prefers-color-scheme all subscribe to
  # that portal, so the whole stack flips in unison. Schedule mirrors
  # hyprsunset above for consistency. `Super+Shift+D` toggles manually.
  services.darkman = {
    enable = true;
    settings = {
      dawn = "06:30";
      dusk = "20:00";
    };
    darkModeScripts = {
      gtk-theme = ''
        ${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"
      '';
      # Refresh the custom/darkman Waybar pill instantly — matches the
      # `signal = 8` on the module so it re-runs `darkman get` and swaps
      # the sun/moon glyph + class without waiting for the 60s poll.
      waybar-refresh = ''
        ${pkgs.procps}/bin/pkill -RTMIN+8 waybar || true
      '';
      # Notify running nvim instances on the host instantly via RPC so the
      # colorscheme switches without waiting for auto-dark-mode's 3-second
      # poll. Default nvim socket layout is $XDG_RUNTIME_DIR/nvim.<pid>.0.
      # <Cmd> fires the Ex command regardless of current mode. VM-side
      # nvim instances aren't reachable from here (separate runtime dir).
      nvim-dark = ''
        for sock in "''${XDG_RUNTIME_DIR:-/run/user/$UID}"/nvim.*.0; do
          [ -S "$sock" ] && ${pkgs.neovim}/bin/nvim --server "$sock" \
            --remote-send '<Cmd>DarkMode<CR>' 2>/dev/null &
        done
        true
      '';
    };
    lightModeScripts = {
      gtk-theme = ''
        ${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme "'prefer-light'"
      '';
      waybar-refresh = ''
        ${pkgs.procps}/bin/pkill -RTMIN+8 waybar || true
      '';
      nvim-light = ''
        for sock in "''${XDG_RUNTIME_DIR:-/run/user/$UID}"/nvim.*.0; do
          [ -S "$sock" ] && ${pkgs.neovim}/bin/nvim --server "$sock" \
            --remote-send '<Cmd>LightMode<CR>' 2>/dev/null &
        done
        true
      '';
    };
  };

  # swayosd — centered OSD popup for brightness and caps-lock changes.
  # Volume OSD lives in the Quickshell `osd` config above, which is
  # event-driven on PipeWire state and so catches volume changes from
  # any source rather than only the keypresses we wire to it.
  services.swayosd.enable = true;

  programs.wlogout = {
    enable = true;
    layout = [
      {
        label = "lock";
        action = "loginctl lock-session";
        text = "Lock";
        keybind = "l";
      }
      {
        label = "logout";
        action = "hyprctl dispatch exit";
        text = "Logout";
        keybind = "e";
      }
      {
        label = "suspend";
        action = "systemctl suspend";
        text = "Suspend";
        keybind = "u";
      }
      {
        label = "hibernate";
        action = "systemctl hibernate";
        text = "Hibernate";
        keybind = "h";
      }
      {
        label = "reboot";
        action = "systemctl reboot";
        text = "Reboot";
        keybind = "r";
      }
      {
        label = "shutdown";
        action = "systemctl poweroff";
        text = "Shutdown";
        keybind = "s";
      }
    ];
    style =
      let
        icons = "${config.home.homeDirectory}/repos/clones/hyprland/catppuccin/wlogout/icons/wleave/mocha/blue";
      in
      ''
        * {
          background-image: none;
          box-shadow: none;
        }
        window {
          background-color: rgba(30, 30, 46, 0.90);
        }
        button {
          border-radius: 0;
          border-color: #89b4fa;
          text-decoration-color: #cdd6f4;
          color: #cdd6f4;
          background-color: #181825;
          border-style: solid;
          border-width: 1px;
          background-repeat: no-repeat;
          background-position: center;
          background-size: 25%;
        }
        button:focus, button:active, button:hover {
          background-color: rgb(48, 50, 66);
          outline-style: none;
        }
        #lock      { background-image: image(url("${icons}/lock.svg")); }
        #logout    { background-image: image(url("${icons}/logout.svg")); }
        #suspend   { background-image: image(url("${icons}/suspend.svg")); }
        #hibernate { background-image: image(url("${icons}/hibernate.svg")); }
        #reboot    { background-image: image(url("${icons}/reboot.svg")); }
        #shutdown  { background-image: image(url("${icons}/shutdown.svg")); }
      '';
  };

  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        font = "FiraCode Nerd Font Mono:size=14";
        prompt = "❯  ";
        lines = 10;
        width = 35;
        layer = "overlay";
      };
      colors = {
        # Catppuccin Mocha, blue accent (github.com/catppuccin/fuzzel)
        background = "1e1e2edd";
        text = "cdd6f4ff";
        prompt = "bac2deff";
        placeholder = "7f849cff";
        input = "cdd6f4ff";
        match = "89b4faff";
        selection = "585b70ff";
        selection-text = "cdd6f4ff";
        selection-match = "89b4faff";
        counter = "7f849cff";
        border = "89b4faff";
      };
      border = {
        width = 2;
        radius = 12;
      };
    };
  };

  services.swaync = {
    enable = true;
    settings = {
      # mpris first so playback controls sit at the top of the panel.
      # Spotify song-change popups arrive via D-Bus notifications and need
      # no extra config — the mpris widget reads the MPRIS2 interface
      # directly for transport controls (prev/play/next/scrub).
      widgets = [
        "mpris"
        "dnd"
        "title"
        "notifications"
      ];
      widget-config.mpris.autohide = true;

      # Append every incoming notification to a plain-text log so content
      # can be retrieved after a popup is dismissed. No match conditions =
      # catches all notifications. Env vars injected by swaync per
      # configModel.vala: SWAYNC_APP_NAME, SWAYNC_SUMMARY, SWAYNC_BODY.
      scripts.log-all.exec = "${swayncLog}";
    };
    # Catppuccin Mocha — expanded from github.com/catppuccin/swaync _theme.scss
    # with mocha palette variables substituted to plain hex values.
    style = ''
      * {
        all: unset;
        font-size: 14px;
        font-family: "FiraCode Nerd Font Mono";
        transition: 200ms;
      }
      trough highlight { background: #cdd6f4; }
      scale { margin: 0 7px; }
      scale trough { margin: 0rem 1rem; min-height: 8px; min-width: 70px; border-radius: 12.6px; }
      trough slider {
        margin: -10px;
        border-radius: 12.6px;
        box-shadow: 0 0 2px rgba(0,0,0,0.8);
        transition: all 0.2s ease;
        background-color: #89b4fa;
      }
      trough slider:hover { box-shadow: 0 0 2px rgba(0,0,0,0.8), 0 0 8px #89b4fa; }
      trough { background-color: #313244; }

      .notification-background {
        box-shadow: 0 0 8px 0 rgba(0,0,0,0.8), inset 0 0 0 1px #45475a;
        border-radius: 12.6px;
        margin: 18px;
        background: #181825;
        color: #cdd6f4;
        padding: 0;
      }
      .notification-background .notification { padding: 7px; border-radius: 12.6px; }
      .notification-background .notification.critical { box-shadow: inset 0 0 7px 0 #f38ba8; }
      .notification .notification-content { margin: 7px; }
      .notification .notification-content overlay { margin: 4px; }
      /* Restore swaync's built-in icon sizes — the leading `all: unset`
         wipes them and catppuccin's upstream _theme.scss doesn't reinstate
         them, so without these rules notification icons render tiny. */
      .notification .notification-content .image { -gtk-icon-size: 64px; }
      .notification .notification-content .app-icon { -gtk-icon-size: 24px; }
      .notification-group-icon { -gtk-icon-size: 32px; }
      .notification-content .summary { color: #cdd6f4; }
      .notification-content .time { color: #a6adc8; }
      .notification-content .body { color: #bac2de; }
      .notification > *:last-child > * { min-height: 3.4em; }
      .notification-background .close-button {
        margin: 7px; padding: 2px; border-radius: 6.3px;
        color: #1e1e2e; background-color: #f38ba8;
      }
      .notification-background .close-button:hover { background-color: #eba0ac; }
      .notification-background .close-button:active { background-color: #f5c2e7; }
      .notification .notification-action {
        border-radius: 7px; color: #cdd6f4;
        box-shadow: inset 0 0 0 1px #45475a;
        margin: 4px; padding: 8px;
        background-color: #313244;
      }
      .notification .notification-action:hover { background-color: #45475a; }
      .notification .notification-action:active { background-color: #585b70; }
      .notification.critical progress { background-color: #f38ba8; }
      .notification.low progress, .notification.normal progress { background-color: #89b4fa; }
      .notification progress, .notification trough, .notification progressbar {
        border-radius: 12.6px; padding: 3px 0;
      }

      .control-center {
        box-shadow: 0 0 8px 0 rgba(0,0,0,0.8), inset 0 0 0 1px #313244;
        border-radius: 12.6px;
        background-color: #1e1e2e;
        color: #cdd6f4;
        padding: 14px;
      }
      .control-center .notification-background {
        border-radius: 7px; box-shadow: inset 0 0 0 1px #45475a; margin: 4px 10px;
      }
      .control-center .notification-background .notification { border-radius: 7px; }
      .control-center .notification-background .notification.low { opacity: 0.8; }
      .control-center .widget-title > label { color: #cdd6f4; font-size: 1.3em; }
      .control-center .widget-title button {
        border-radius: 7px; color: #cdd6f4;
        background-color: #313244; box-shadow: inset 0 0 0 1px #45475a; padding: 8px;
      }
      .control-center .widget-title button:hover { background-color: #45475a; }
      .control-center .widget-title button:active { background-color: #585b70; }
      .control-center .notification-group { margin-top: 10px; }
      .control-center .notification-group:focus .notification-background { background-color: #313244; }
      scrollbar slider { margin: -3px; opacity: 0.8; }
      scrollbar trough { margin: 2px 0; }

      .widget-dnd { margin-top: 5px; border-radius: 8px; font-size: 1.1rem; }
      .widget-dnd > switch { font-size: initial; border-radius: 8px; background: #313244; box-shadow: none; }
      .widget-dnd > switch:checked { background: #89b4fa; }
      .widget-dnd > switch slider { background: #45475a; border-radius: 8px; }

      .widget-mpris-player { background: #313244; border-radius: 12.6px; color: #cdd6f4; }
      .mpris-overlay { background-color: #313244; opacity: 0.9; padding: 15px 10px; }
      .widget-mpris-album-art { -gtk-icon-size: 100px; border-radius: 12.6px; margin: 0 10px; }
      .widget-mpris-title { font-size: 1.2rem; color: #cdd6f4; }
      .widget-mpris-subtitle { font-size: 1rem; color: #bac2de; }
      .widget-mpris button { border-radius: 12.6px; color: #cdd6f4; margin: 0 5px; padding: 2px; }
      .widget-mpris button image { -gtk-icon-size: 1.8rem; }
      .widget-mpris button:hover { background-color: #313244; }
      .widget-mpris button:active { background-color: #45475a; }
      .widget-mpris button:disabled { opacity: 0.5; }
    '';
  };

  # hyprpolkitagent works out of the box (no config needed) — it just listens
  # on the polkit D-Bus and pops up a password prompt when an app asks for
  # elevated privileges (sudo GUI prompts, NetworkManager VPN auth, etc.).
  services.hyprpolkitagent.enable = true;

  # gnome-keyring-daemon as a systemd user service, providing the secret
  # service and the SSH agent for the session. PAM (configured at the
  # system level by services.gnome.gnome-keyring in nixos/common/hyprland-desktop.nix)
  # unlocks the keyring at GDM login via pam_gnome_keyring.so, talking to
  # this long-lived daemon over $XDG_RUNTIME_DIR/keyring/control. Under
  # GNOME, gnome-session keeps the daemon alive with the same components;
  # under Hyprland nothing does, so an ephemeral D-Bus-activated daemon
  # (without the PAM password and without the ssh component) prompts for
  # the password the first time anything touches libsecret — e.g. git's
  # ssh, asking the agent for the key passphrase. Components match what
  # gnome-session starts (pkcs11 isn't needed here).
  services.gnome-keyring = {
    enable = true;
    components = [
      "secrets"
      "ssh"
    ];
  };

  # Tray applets shown in Waybar's tray module. The home-manager service
  # modules pull in the right package and wire systemd user units bound to
  # the wayland session — cleaner than spawning them from exec-once.
  services.network-manager-applet.enable = true;
  services.blueman-applet.enable = true;

  # cliphist — clipboard history daemon. Module default is 500 entries;
  # capped lower here for shorter retention.
  services.cliphist = {
    enable = true;
    extraOptions = [
      "-max-dedupe-search"
      "10"
      "-max-items"
      "50"
    ];
  };
}
