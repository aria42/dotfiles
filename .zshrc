# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="af-magic"
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
plugins=(git git-flow ruby rbenv coffee lein osx sublime)

source $ZSH/oh-my-zsh.sh

# exports and sets
export PATH=/usr/local/bin:/usr/local:/usr/local/sbin:~/.cabal/bin:~/julia:/usr/local/share/npm/bin:/Applications/Racket/bin:/usr/local/Cellar/smlnj/110.75/libexec/bin:~/bin:~/Applications/chromedriver:/usr/bin:/bin:/usr/sbin:/sbin:~/GoogleDrive/experiments/haskell-ios/ghc-ios-scripts
export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"
FC=/usr/local/bin/gfortran
TERM=xterm-256color
WORKON_HOME=$HOME/.virtualenvs
export RLWRAP_HOME="$HOME/.rlwrap"

# extra path exports
export PATH="$HOME/.rbenv/bin:$PATH"

# sources
source /usr/local/share/python/virtualenvwrapper.sh

# other sources
if [ -f ~/.localrc/.zsh_aliases ]; then
    source ~/.localrc/.zsh_aliases
fi

# aliases
alias git-root='cd $(git rev-parse --show-toplevel)'
alias gfz='git fetch'
alias gs='git status'
alias ppj='python -m json.tool'
alias subl-packages='~/Library/Application\ Support/Sublime\ Text\ 2/Packages'
alias sml='rlwrap sml'
alias emacst='emacsclient -t -a vim'
alias spot="osascript ~/SpotifyControl/SpotifyControl.scpt"

# functions
skill () {
    kill -9 `ps ax | grep $1 | grep -v grep | awk '{print $1}'`
}

fis () {
    find . -type f -name $1
}

xc () {
    xcode_proj=`find . -name "*.xc*" -d 1 | sort -r | head -1`

    if [ `echo -n $xcode_proj | wc -m` -eq 0 ]
    then
        echo "No xcworkspace/xcodeproj file found in the current directory."
    else
        echo "Found $xcode_proj"
        open $xcode_proj
    fi
}

. `brew --prefix`/etc/profile.d/z.sh

# evals
eval "$(rbenv init -)"

# runs
fortune | cowsay -f ghostbusters

# Disable AutoCorrect
unsetopt correct_all
