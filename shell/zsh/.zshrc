# =========================== History-Related Config ===========================
HISTFILE="$HOME/.zhistory"
HISTSIZE=10000000
SAVEHIST=10000000

setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing nonexistent history.

# ==============================================================================
setopt autocd nomatch
unsetopt beep
bindkey -e

# The following lines were added by compinstall
zstyle :compinstall filename '/home/joe/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Use antidote
source '/usr/share/zsh-antidote/antidote.zsh'
antidote load ~/git/dotfiles/shell/zsh/.zsh_plugins.txt

# Variable to hold a newline so that we can have multi-line prompts. zsh is
# kinda weird about them (sending them down in to another line, but I can't
# actually see what that new line is (it's off the screen.))
NEWLINE=$'\n'
PS1="%F{green}[%*]%F{yellow}%n@%m:%F{red}%~${NEWLINE}%F{cyan}%(!.#.$)%f "

# ================================= $PATH Setup ================================
export PATH="$HOME/.build/bin:$HOME/.local/bin:$PATH"
export LD_LIBRARY_PATH="$HOME/.build/lib/:$LD_LIBRARY_PATH"

# ================================ Movement Keys ===============================
# A few special movement keys don't work in Zsh like they did in Bash Solution
# from:
# https://www.linuxquestions.org/questions/linux-software-2/ins-end-delete-keys-are-mischevious-297024/
# Rebind HOME and END to do the decent thing:
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
case $TERM in (xterm*)
bindkey '\eOH' beginning-of-line
bindkey '\eOF' end-of-line
esac

# To discover what keycode is being sent, hit ^v and then the key you want to
# test.

# And DEL too, as well as PGDN and insert:
bindkey '\e[3~' delete-char
bindkey '\e[6~' end-of-history
bindkey '\e[2~' redisplay

# Now bind pgup to paste the last word of the last command,
bindkey '\e[5~' insert-last-word

# Use CTRL + <LEFT|RIGHT> to move between words
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
# Use ALT + <LEFT|RIGHT> to move between words
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word

# =================================== Aliases ==================================
alias ls='ls --color=auto --group-directories-first'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
