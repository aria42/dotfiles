# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="agnoster"
DEFAULT_USER="zeeshanlakhani"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=10

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git git-flow ruby rbenv autojump coffee lein osx sublime)

source $ZSH/oh-my-zsh.sh

# exports and sets
export PATH=/usr/local/bin:/usr/local:/usr/local/sbin:/usr/local/share/python:/Users/zeeshanlakhani/.cabal/bin:/usr/local/Cellar/smlnj/110.75/libexec/bin:/Users/zeeshanlakhani/bin:/Users/zeeshanlakhani/Applications/chromedriver:/usr/bin:/bin:/usr/sbin:/sbin:/~/z/z.sh
export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"
FC=/usr/local/bin/gfortran
TERM=xterm-256color
WORKON_HOME=$HOME/.virtualenvs

# extra path exports
export PATH="$HOME/.rbenv/bin:$PATH"
export PATH="$HOME/.rbenv/bin:$PATH"

# sources
source /usr/local/share/python/virtualenvwrapper.sh

# other sources
if [ -f ~/.zsh_aliases ]; then
    source ~/.zsh_aliases
fi

# aliases
alias git-root='cd $(git rev-parse --show-toplevel)'
alias gfz='git fetch'
alias gs='git status'
alias ppj='python -m json.tool'
alias subl-packages='~/Library/Application\ Support/Sublime\ Text\ 2/Packages'

# functions
skill () {
    kill -9 `ps ax | grep $1 | grep -v grep | awk '{print $1}'`
}

fis () {
    find . -type f -name $1
}

# evals
eval "$(rbenv init -)"

# runs
fortune | cowsay -f ghostbusters
