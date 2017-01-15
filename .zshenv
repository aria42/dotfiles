# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Path to your prezto configuration.
ZSH=$HOME/.zprezto

DEFAULT_USER="zeeshanlakhani"

# Use modern completion system
autoload -Uz compinit
compinit

# History
export HISTFILE=~/.zsh_history

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

# exports and sets
export PATH=~/.local/bin:/usr/local/bin:/usr/local:/usr/local/sbin:~/.erln8.d/bin:~/Library/Haskell/bin:~/.cabal/bin:~/julia:/usr/local/share/npm/bin:/Applications/Racket/bin:/usr/local/Cellar/smlnj/110.75/libexec/bin:~/bin:~/Applications/chromedriver:/usr/bin:/bin:/usr/sbin:/sbin:/mono64/bin:/Applications/Postgres.app/Contents/Versions/latest/bin:~/riak_test/utils/riak_test.zsh:/usr/texbin:~/.jenv/bin:~/lfe/bin:~/kjell:~/erlang/libs/cuter:/opt/X11/bin:~/$PATH
export PYTHONPATH="/usr/local/lib/python3/site-packages:$PYTHONPATH"
FC=/usr/local/bin/gfortran
TERM=xterm-256color
WORKON_HOME=$HOME/.virtualenvs
export RLWRAP_HOME="$HOME/.rlwrap"
export GTAGSCONF=/usr/local/share/gtags/gtags.conf
export GTAGSLABEL=ctags

# extra path exports
export PATH="$HOME/.rbenv/bin:$PATH"

# sugesstions
## for mac
## source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# other sources
if [ -f ~/.localrc/.zsh_aliases ]; then
    source ~/.localrc/.zsh_aliases
fi

# aliases
alias git-root='cd $(git rev-parse --show-toplevel)'
alias gfz='git fetch'
alias gs='git status'
alias ppj='python -m json.tool'
alias sml='rlwrap sml'
alias elcpurge='find ~/.emacs.d -name "*.elc" -print | xargs rm -f'
alias elpapurge='rm -Rf ~/.emacs.d/elpa/*'
alias readlink=greadlink
alias cabalupgrades="cabal list --installed  | egrep -iv '(synopsis|homepage|license)'"
alias py='python3'
alias gcc='gcc-6'
alias mg='mg -n'

## for mac-emacs
# alias emacsd='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
# alias em='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait'
# alias emn='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c --no-wait'
# alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
# alias killem='pkill -TERM -u $USER Emacs'

## for linux-emacs
alias emacsd='/usr/bin/emacs --daemon'
alias em='/usr/bin/emacsclient --no-wait'
alias emn='/usr/bin/emacsclient -c --no-wait'
alias emacs='/usr/bin/emacs'
alias killem='pkill -TERM -u $USER emacs'

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

# path to z (better than j lib)
. ~/CLI/z/z.sh

# evals
eval "$(rbenv init -)"

# runs
fortune | cowsay -f ghostbusters

# Disable AutoCorrect
unsetopt correct_all

# Cabal

# ghc-pkg-reset
# Removes all installed GHC/cabal packages, but not binaries, docs, etc.
# Use this to get out of dependency hell and start over, at the cost of some rebuilding time.
ghc-pkg-reset () {
    read -p 'erasing all your user ghc and cabal packages - are you sure (y/n) ? ' ans
    test x$ans == xy && ( \
        echo 'erasing directories under ~/.ghc'; rm -rf `find ~/.ghc -maxdepth 1 -type d`; \
        echo 'erasing ~/.cabal/lib'; rm -rf ~/.cabal/lib; \
        # echo 'erasing ~/.cabal/packages'; rm -rf ~/.cabal/packages; \
        # echo 'erasing ~/.cabal/share'; rm -rf ~/.cabal/share; \
    )
}

# set up amazon web services credentials
export AWS_CONFIG_FILE="${HOME}/.aws/config"
if [[ -f $AWS_CONFIG_FILE ]]; then
    export AWS_ACCESS_KEY=$(awk -F= '/aws_access_key_id/ { print $2 }' $AWS_CONFIG_FILE)
    export AWS_SECRET_KEY=$(awk -F= '/aws_secret_access_key/ { print $2 }' $AWS_CONFIG_FILE)
    export AWS_ACCESS_KEY_ID="$AWS_ACCESS_KEY"
    export AWS_SECRET_ACCESS_KEY="$AWS_SECRET_KEY"
fi

# set JAVA_HOME
# export JAVA_HOME=$(/usr/libexec/java_home)

# eval "$(jenv init -)"

# OPAM configuration
. /home/zeeshanlakhani/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# elixir verision mgr
test -s "$HOME/.kiex/scripts/kiex" && source "$HOME/.kiex/scripts/kiex"

# source virutalenwrapper
source /usr/local/bin/virtualenvwrapper.sh

# pdflatex
export PATH="/Library/TeX/texbin:$PATH"
