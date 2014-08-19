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
plugins=(git git-flow ruby rbenv coffee lein osx sublime virtualenv postgres)

source $ZSH/oh-my-zsh.sh

# exports and sets
export PATH=/usr/local/bin:/usr/local:/usr/local/sbin:~/Library/Haskell/bin:$PATH:~/.cabal/bin:~/julia:/usr/local/share/npm/bin:/Applications/Racket/bin:/usr/local/Cellar/smlnj/110.75/libexec/bin:~/bin:~/Applications/chromedriver:/usr/bin:/bin:/usr/sbin:/sbin:/mono64/bin:/Applications/Postgres93.app/Contents/MacOS/bin
export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"
FC=/usr/local/bin/gfortran
TERM=xterm-256color
WORKON_HOME=$HOME/.virtualenvs
export RLWRAP_HOME="$HOME/.rlwrap"

# extra path exports
export PATH="$HOME/.rbenv/bin:$PATH"

# sources
source "$(which virtualenvwrapper.sh)"

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
alias spot='osascript ~/SpotifyControl/SpotifyControl.scpt'
alias emacsd='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
alias em='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n'
alias elcpurge='find ~/.emacs.d -name "*.elc" -print | xargs rm -f'
alias elpapurge='rm -Rf ~/.emacs.d/elpa/*'

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

# OPAM configuration
. /Users/zeeshanlakhani/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

eval `opam config env`

# Cabal

# unregister broken GHC packages. Run this a few times to resolve dependency rot in installed packages.
# ghc-pkg-clean -f cabal/dev/packages*.conf also works.
function ghc-pkg-clean() {
    for p in `ghc-pkg check $* 2>&1  | grep problems | awk '{print $6}' | sed -e 's/:$//'`
    do
        echo unregistering $p; ghc-pkg $* unregister $p
    done
}

alias readlink=greadlink
alias cabalupgrades="cabal list --installed  | egrep -iv '(synopsis|homepage|license)'"

# set up amazon web services credentials
export AWS_CONFIG_FILE="${HOME}/.aws/config"
if [[ -f $AWS_CONFIG_FILE ]]; then
    export AWS_ACCESS_KEY=$(awk -F= '/aws_access_key_id/ { print $2 }' $AWS_CONFIG_FILE)
    export AWS_SECRET_KEY=$(awk -F= '/aws_secret_access_key/ { print $2 }' $AWS_CONFIG_FILE)
    export AWS_ACCESS_KEY_ID="$AWS_ACCESS_KEY"
    export AWS_SECRET_ACCESS_KEY="$AWS_SECRET_KEY"
fi
