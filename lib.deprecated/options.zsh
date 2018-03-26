# Treat these characters as part of a word.
WORDCHARS='_-*?[]~&.;!#$%^(){}<>'

# When Ctrl-w treat slashes, dots and few other things as delimiters too
autoload -U select-word-style
select-word-style bash

zmodload -i zsh/complist

set -o emacs

#
# History
#

setopt BANG_HIST                 # Don't treat '!' specially during expansion.
# setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
# setopt APPEND_HISTORY            # Appends history to history file on exit
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
# setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing non-existent history.

#
# Completion
#

setopt COMPLETE_IN_WORD    # Complete from both ends of a word.
setopt ALWAYS_TO_END       # Move cursor to the end of a completed word.
setopt PATH_DIRS           # Perform path search even on command names with slashes.
setopt AUTO_MENU           # Show completion menu on a successive tab press.
setopt AUTO_LIST           # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH    # If completed parameter is a directory, add a trailing slash.
setopt AUTO_PARAM_KEYS
unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.
unsetopt FLOW_CONTROL      # Disable start/stop characters in shell editor.
unsetopt COMPLETE_ALIASES  # Completion for aliases
unsetopt CASE_GLOB

#
# General
#

setopt BRACE_CCL          # Allow brace character class list expansion.
setopt COMBINING_CHARS    # Combine zero-length punctuation chars (accents) with the base char
setopt RC_QUOTES          # Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'.
setopt HASH_LIST_ALL
unsetopt MAIL_WARNING     # Don't print a warning message if a mail file has been accessed.
unsetopt CORRECT_ALL
unsetopt NOMATCH
unsetopt BEEP             # Hush now, quiet now.

#
# Jobs
#

setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt AUTO_RESUME        # Attempt to resume existing job before creating a new process.
setopt NOTIFY             # Report status of background jobs immediately.
unsetopt BG_NICE          # Don't run all background jobs at a lower priority.
unsetopt HUP              # Don't kill jobs on shell exit.
unsetopt CHECK_JOBS       # Don't report on jobs when shell exit.

#
# Directories
#

DIRSTACKSIZE=9
setopt AUTO_CD              # Auto changes to a directory without typing cd.
setopt AUTO_PUSHD           # Push the old directory onto the stack on cd.
setopt PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME        # Push to home directory when no argument is given.
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt MULTIOS              # Write to multiple descriptors.
setopt EXTENDED_GLOB        # Use extended globbing syntax.
unsetopt CLOBBER            # Don't overwrite existing files with > and >>.
unsetopt GLOB_DOTS          # Don't include dotfiles in globbing
unsetopt AUTO_NAME_DIRS     # Don't add variable-stored paths to ~ list.
# Use >! and >>! to bypass.

#
# Misc
#

setopt CDABLEVARS
setopt PROMPT_SUBST
setopt PROMPTPERCENT
setopt PROMPTSUBST
setopt INTERACTIVECOMMENTS # enable the bash-style comments
unsetopt NOMATCH          # remove HEAD^ escaping madness
unsetopt FLOWCONTROL
