# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob
# Append to the Bash history file, rather than overwriting it
shopt -s histappend
# Autocorrect typos in path names when using `cd`
shopt -s cdspell
# Check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize
# Extended pattern matching
shopt -s extglob
# Node Completion - Auto-generated, do not touch.
shopt -s progcomp

# no duplicates in bash history and ignore same sucessive entries.
set match-hidden-files off
set page-completions off
set completion-query-items 350

# Enable some Bash 4 features when possible:
# `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
shopt -s autocd 2>/dev/null
# Recursive globbing, e.g. `echo **/*.txt`
shopt -s globstar 2>/dev/null
