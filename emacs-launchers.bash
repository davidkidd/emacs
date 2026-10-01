# Shell entry points for launching Emacs, one per configuration.
# Source this file from ~/.bashrc:
#
# if [ -r "$HOME/.emacs.d/emacs-launchers.bash" ]; then
#    . "$HOME/.emacs.d/emacs-launchers.bash"
# fi
#
# Two independent launchers live here:
#
# 1. Embasic (e, d, D) - a minimal Emacs configured in early-init.el. It uses
#    only built-ins, skips packages, site and user init, and starts fast. It is
#    meant for quick edits (like nano) and for Dired as a two-pane file manager.
#
#      e [files]     edit
#      d             Dired here
#      D [directory] dual-pane Dired, optional dir arg in right-hand pane
#
#    Set use_embasic_server below to choose between reusing a warm server and
#    starting a fresh process each time.
#
# 2. just agent-shell - opens straight into agent-shell with a few Emacs extras
#    in a single window. Intended for simple, short-lived, chat-oriented tasks.
#    Use the full Emacs config for complex tasks.
#
#      ai            launch into agent-shell
#

# Embasic
# Reuse a warm embasic server when true; otherwise start a standalone process.
# The server is faster to re-enter, but it is shared state: buffers, window
# layout and C-x C-c all belong to the daemon rather than your terminal.
# Set to false for a more conventional environment.
use_embasic_server=false

embasic() {
    local embasic_init="$HOME/.emacs.d/early-init.el"
    if [ ! -r "$embasic_init" ]; then
        printf 'Embasic: cannot read %s. Install the Embasic early-init.el first.\n' "$embasic_init" >&2
        return 1
    fi
    if [ "$use_embasic_server" = true ]; then
        # Start the daemon only if nothing answers on the socket; emacsclient
        # -a cannot do this itself, it gives up before the daemon is listening.
        command emacsclient -s embasic -a false -e t >/dev/null 2>&1 || command emacs --daemon=embasic -basic >/dev/null 2>&1
        command emacsclient -s embasic -a false -t "$@"
    else
        command emacs -nw -basic "$@"
    fi
}
e() { embasic "$@"; }
alias d='e .'
D() (
    if (( $# > 1 )); then
        printf 'Usage: D [directory]\n' >&2
        return 2
    fi
    EMACS_DIRED_RIGHT_DIRECTORY=$(cd -- "${1:-.}" && pwd -P) || return
    export EMACS_DIRED_RIGHT_DIRECTORY
    e --eval '(embasic-dual-dired (or (getenv "EMACS_DIRED_RIGHT_DIRECTORY" (selected-frame)) (getenv "EMACS_DIRED_RIGHT_DIRECTORY")))'
)

# AI
# Launch a full Emacs that goes straight into Agent Shell in a single window.
# Only the init files listed in my/ai-only-init are loaded.
# -q is required: without it Emacs loads init.el as the user init file before
# --eval runs, so every extra init loads and the flags arrive too late.
# custom.el is loaded first so my/ai-only-init exists when --eval reads it.
# Packages are activated explicitly because -q skips package-initialize, and
# init-general.el needs transient on the load-path for agent-shell, rg, magit.
ai() {
    local emacs_dir="$HOME/.emacs.d"
    command emacs -q -nw \
        -l "$emacs_dir/custom/custom.el" \
        --eval '(condition-case nil (package-initialize) (error nil))' \
        --eval '(setq my/extra-init-override my/ai-only-init
                      launch-agent-on-startup t
                      my/agent-shell-solo t)' \
        -l "$emacs_dir/init.el" "$@"
}
