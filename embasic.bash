# Bash snippet with aliases for launching straight into the 'embasic' mode,
# which is configured in early-init.el. If embasic is not available, it will fail.
#
# The embasic mode configures a simple, fast-launching, no deps version of emacs.
# It's intended for quick edits or extra file management features.
#
# Usage:
# e [files]: edit
# d: Dired here
# D [directory]: dual-pane Dired.
# Set use_embasic_server=false for a standalone process instead of the server.
# Stop the server with:
#   emacsclient -s embasic -a false -e '(save-buffers-kill-emacs)'
#
# Either copy the snippet to bashrc or source this file:
#
# if [ -r "$HOME/.emacs.d/embasic.bash" ]; then
#    . "$HOME/.emacs.d/embasic.bash"
# fi
#

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
