# Aliases

alias lst='ls -lhtF --color=auto | more'
alias o='less'
alias ld='cd $(ls -dt */ | awk 'NR==0')'
alias ll='ls -lh --color=auto'
alias mneBrowse='mne_browse_raw --digtrig STI101'
alias tmux='TERM=xterm-256color tmux'
alias cl='column -t -s,'
alias lr='ls -ltr'

## Git aliases
alias gl="git log --graph --abbrev-commit --decorate --date=relative \
    --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) \
    %C(white)%s%C(reset) %C(dim white)-%an%C(reset)%C(bold yellow)%d%C(reset)' \
    --all"
alias gs='git status '
alias ga='git add '
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
alias go='git checkout '
alias gk='gitk --all&'
alias gsw='git show ' 


## Python aliases
alias ipl='ipython --pylab=QT'
alias ipqt='ipython qtconsole --colors=Linux --pylab=QT'
alias ipex='ipython qtconsole --colors=Linux --pylab=QT --existing'
