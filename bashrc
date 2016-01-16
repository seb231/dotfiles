# add bin to path
export PATH=$PATH:/home/mje/bin/

# Toolboxes setup
# export FSLDIR=/usr/share/fsl/5.0
export FREESURFER_HOME=/home/mje/Toolboxes/freesurfer
export MNE_ROOT=/home/mje/Toolboxes/MNE

source $MNE_ROOT/bin/mne_setup_sh
# source $MNE_ROOT/bin/mne_setup_matlab_sh
source $FREESURFER_HOME/SetUpFreeSurfer.sh
# Aliases
e() { emacsclient "$@" & }

alias tmux='TERM=xterm-256color tmux'
alias enw='emacs -nw'
alias mRP='sudo mount -t cifs -o username=mje,uid=1000 //hyades00.pet.auh.dk/projects/MINDLAB2011_24-MEG-readiness /home/mje/mnt/RP_meg'
alias mHyp='sudo mount -t cifs -o username=mje,uid=1000 //hyades00.pet.auh.dk/projects/MINDLAB2013_18-MEG-HypnosisAnarchicHand /home/mje/mnt/Hyp_meg'
alias mCAA='sudo mount -t cifs -o username=mje,uid=1000 //hyades00.pet.auh.dk/projects/MINDLAB2015_MEG-CorticalAlphaAttention /home/mje/mnt/caa'
alias cl='column -t -s,'
alias lr='ls -ltr'

# DNC related 
alias rdk_ssh="rdesktop -k da -g 1580x850 localhost:3395"
alias rdk_dnc="rdesktop -k da -g 1580x850 isis"
alias isis_ssh="ssh -p 10027 -l mje localhost"    
alias hyades02_ssh="ssh -p 10024 -l mje localhost"    

## Python aliases
#alias condaAct='source condaActivate'
#alias condaDeact='source condaDeactivate'
alias ipl='ipython --pylab=QT'
alias ipqt='ipython qtconsole --colors=Linux --pylab=QT'
alias ipex='ipython qtconsole --colors=Linux --pylab=QT --existing'
alias py_server='python -m SimpleHTTPServer 8000'

#### PROMT ####
_PS1 ()
{
    local PRE= NAME="$1" LENGTH="$2";
    [[ "$NAME" != "${NAME#$HOME/}" || -z "${NAME#$HOME}" ]] &&
        PRE+='~' NAME="${NAME#$HOME}" LENGTH=$[LENGTH-1];
    ((${#NAME}>$LENGTH)) && NAME="/...${NAME:$[${#NAME}-LENGTH+4]}";
    echo "$PRE$NAME"
}


if [[ $HOSTNAME == 'wintermute' ]]
then
    export hn='wm'

else
    export hn=$HOSTNAME
fi


PS1='[\u@$hn: $(_PS1 "$PWD" 15)] '

# setup git completion
source ~/.git-completion.bash

stty stop undef # to unmap ctrl-s
TERM=screen-256color

# added by Anaconda 1.7.0 installer
export PATH="/home/mje/Toolboxes/anaconda/bin:$PATH"

#  source aliases
source ~/.bash_aliases

# clear the terminal window
# clear
