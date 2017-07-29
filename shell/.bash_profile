source "$HOME"/.utils.sh

load shell/lib/options.bash
load shell/lib/env.sh
# Remove duplicates in PATH
cleanpath
loadall env.sh
loadall env.bash

# cache rbenv init - --no-rehash
# cache pyenv init - --no-rehash
