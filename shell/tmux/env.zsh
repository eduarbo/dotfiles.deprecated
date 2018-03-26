export TMUX_HOME="$XDG_CONFIG_HOME/tmux"
export TMUX_PLUGINS_HOME="$XDG_DATA_HOME/tmux/plugins"
export TMUX_PLUGIN_MANAGER_PATH="$TMUX_PLUGINS_HOME/tpm"

export TMUXIFIER="$XDG_DATA_HOME/tmuxifier"
export TMUXIFIER_LAYOUT_PATH="$TMUX_HOME/layouts"

path=( $TMUXIFIER/bin $path )

#
_cache tmuxifier init -
