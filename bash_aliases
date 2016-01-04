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
alias ipqt='ipython qtconsole --style=fruity --pylab=QT'
alias ipex='ipython qtconsole --style=fruity --pylab=QT --existing'


## Freesurfer
alias fv='freeview \!*'
alias sd='export SUBJECTS_DIR=`pwd`'
alias subdir='echo $SUBJECTS_DIR'
alias fshome='echo $FREESURFER_HOME'
alias csubdir='cd $SUBJECTS_DIR'
alias cfshome='cd $FREESURFER_HOME'

function fvd () { command freeview -v "$SUBJECTS_DIR/$@"/mri/brainmask.mgz; }
function fvPial () { if [ $# -eq 0 ]
    then
	echo "No subject supplied"
    else
	command freeview \
	    -v "$SUBJECTS_DIR/$@"/mri/T1.mgz \
	    "$SUBJECTS_DIR/$@"/mri/brainmask.mgz \
	    -f "$SUBJECTS_DIR/$@"/surf/lh.white:edgecolor='red' \
	    "$SUBJECTS_DIR/$@"/surf/rh.white:edgecolor='red' \
	    "$SUBJECTS_DIR/$@"/surf/lh.pial:edgecolor='blue' \
	    "$SUBJECTS_DIR/$@"/surf/rh.pial:edgecolor='blue'
fi; }

function fvBemCheck () { if [ $# -eq 0 ]
    then
	echo "No subject supplied"
    else
	command freeview -v "$SUBJECTS_DIR/$@"/mri/T1.mgz \
	    -f "$SUBJECTS_DIR/$@"/bem/"$@"_brain.surf:edgecolor='red' \
	    "$SUBJECTS_DIR/$@"/bem/"$@"_inner_skull.surf:edgecolor='blue' \
	    "$SUBJECTS_DIR/$@"/bem/"$@"_outer_skull.surf:edgecolor='green'
fi ;}

function fvRcCheck() { if [ $# -eq 0 ]
    then
	   echo "No subject supplied"
    else
    freeview \
	    -v "$SUBJECTS_DIR/$@"/mri/T1.mgz \
	    "$SUBJECTS_DIR/$@"/mri/brainmask.mgz \
	    "$SUBJECTS_DIR/$@"/mri/wm.mgz \
        "$SUBJECTS_DIR/$@"/mri/aseg.mgz:colormap=lut:opacity=0.2 \
	    -f "$SUBJECTS_DIR/$@"/surf/lh.white:edgecolor='red' \
	    "$SUBJECTS_DIR/$@"/surf/rh.white:edgecolor='red' \
	    "$SUBJECTS_DIR/$@"/surf/lh.pial:edgecolor='blue' \
	    "$SUBJECTS_DIR/$@"/surf/rh.pial:edgecolor='blue'
fi; }

function fvBrainmask() { if [ $# -eq 0 ]
    then
	   echo "No subject supplied"
    else
    freeview \
	    -v "$SUBJECTS_DIR/$@"/mri/T1.mgz \
	    "$SUBJECTS_DIR/$@"/mri/brainmask.mgz
fi; }

