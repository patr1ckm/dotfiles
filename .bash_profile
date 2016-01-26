alias skim='open -a Skim'
alias emacsg='open -a Aquamacs'
alias ls='ls -G'
alias sl='ssh pmille13@lubkelab.psych.nd.edu'
alias s1='ssh pmille13@crcfe01.crc.nd.edu'
alias s2='ssh pmille13@crcfe02.crc.nd.edu'
alias pycon='ipython nbconvert --to=python'


trsh() { mv $@ ~/.Trash; }
PATH=$PATH:~/bin ; export PATH

## this is where gem installed binaries are located
export PATH=/usr/local/opt/ruby/bin:$PATH

## this is where the cabal installed binaries are
export PATH="$HOME/Library/Haskell/bin:$PATH"

export PERL5LIB=/Users/pmille13/Software/vcftools/src/perl/

export PATH="$PATH:/Applications/Julia-0.4.2.app/Contents/Resources/julia/bin"

#export PATH="/Users/pmille13/Library/Application Support/GoodSync":$PATH
##
# Your previous /Users/pmille13/.bash_profile file was backed up as /Users/pmille13/.bash_profile.macports-saved_2012-09-13_at_18:09:24
##

# MacPorts Installer addition on 2012-09-13_at_18:09:24: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

export PATH=/usr/local/bin:$PATH


# added by Anaconda 2.0.1 installer
#export PATH="/Users/pmille13/anaconda/bin:$PATH"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
export PATH=/usr/local/redis/bin:$PATH
