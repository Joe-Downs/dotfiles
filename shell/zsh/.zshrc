# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=1000
setopt autocd nomatch
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
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


# =================================== Aliases ==================================
alias ls='ls --color=auto --group-directories-first'
