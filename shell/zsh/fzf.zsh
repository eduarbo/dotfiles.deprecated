# Dedicated completion key
# https://github.com/junegunn/fzf/wiki/Configuring-fuzzy-completion#dedicated-completion-key
export FZF_COMPLETION_TRIGGER=''
bindkey '^T' fzf-completion  # Trigger context-aware fuzzy completion with CTRL-T
bindkey '^I' $fzf_default_completion  # Retain the default behavior of TAB key

# automatically selects the item if there's only one
export FZF_CTRL_T_OPTS="--select-1 --exit-0"

# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --color=always --exclude .git'

_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}
