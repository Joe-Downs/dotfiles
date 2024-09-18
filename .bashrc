# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if [ $USER == "root" ]; then
    prompt_char="#"
else
    prompt_char="$"
fi

# Force the terminal mode to fix tmux and GNU screen causing emacs to show
# colors incorrectly.
export TERM=xterm-256color

# Commandline Prompt (See BASH(1) man page, section PROMPTING for escape
# characters)
export PS1="\[\e[1;32m\][\t]\[\e[0;33m\]\u@$HOSTNAME:\[\e[31m\]\w\[\e[0m\]
\[\e[1;36m\]$prompt_char\[\e[0m\] "

# Set ~/go as $GOPATH
if [ -x ~/go ]; then
    export GOPATH="~/go"
fi

# Add cargo binary dir to PATH
if [ -x ~/.cargo/bin ]; then
    export PATH=~/.cargo/bin:$PATH
fi

# Add ~/.emacs.d/bin to the $PATH so that we can call 'doom' directly
export PATH=$HOME/.emacs.d/bin:$PATH

# Add ~/git/dotfiles/shell to the path so we can call our custom scripts
# directly.
export PATH=$HOME/git/dotfiles/shell:$PATH

# Add ~/.local/bin to $PATH
export PATH=$HOME/.local/bin:$PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi


# User specific aliases and functions

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

source /usr/share/doc/fzf/examples/key-bindings.bash
source /usr/share/doc/fzf/examples/completion.bash

# enable color support of ls (and group directories first) and also add handy
# aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto --group-directories-first'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Syntax highlight when printing out file contents
alias ccat='pygmentize -O style=rrt -g'

# If we're on Linux, alias 'xdg-open' to be just 'open'. This is hopefully to
# avoid breaking it on Mac, since they have 'open' by default (if I ever even go
# on one...). Bash gives us the variable, $OSTYPE so we'll check if that is
# "linux-gnu"
if [ $OSTYPE == "linux-gnu" ]; then
    alias open='xdg-open'
fi

# Alias a command "weather" to do `curl wttr.in?mF` (get the weather for my
# current location in metric and don't add the 'follow' line)
# https://wttr.in/:help.
alias weather="curl wttr.in?mF"

# Alias a command to get the current timestamp in YYYY-MM-DD-hhmmss format
alias timestamp="date +%F-%H%M%S"

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# Add thefuck alias for correcting typos in command calls
eval $(thefuck --alias)

print_motd=1
# Programs that need to be installed for the MOTD to work correctly
motd_deps="cowsay figlet fortune lolcat"

for dep in $motd_deps; do
    if [[ `type -t $dep` != "file" ]]; then
	# If a command isn't installed, set the variable to false; i.e., don't
	# print out the MOTD
	print_motd=0
	echo "$dep is missing"
    fi
done

if [[ $print_motd -eq 1 ]]; then
    (figlet -c Welcome; fortune) | cowsay -f tux -n | lolcat
else
    echo "Couldn't print the MOTD"
fi
