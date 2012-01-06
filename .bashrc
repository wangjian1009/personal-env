# ------------------------
# prompt
# ------------------------
# export PS1="[\w] "
export LC_ALL=zh_CN.gbk
export PS1="sailor% "

if [ "$PS" == "" ] ; then
    export PS1="sailor\$ "
    if [ "$TERM" == "xterm" ] ; then
        export PS1="\[\e[34m\]sailor\$ \[\e[0m\]"
    elif [ "$TERM" == "cygwin" ] ; then
        export PS1="\[\e[32;1m\]sailor\$ \[\e[0m\]"
    fi
fi

if [ "$TERM" == "xterm" ] ; then
    PROMPT_COMMAND='echo -ne "\e]0;${HOSTNAME} - ${PWD}\007"'
fi

# ------------------------
# path
# ------------------------
PATH=.
PATH=$PATH:/cygdrive/c/programs/bin
PATH=$PATH:/usr/x11R6/bin
PATH=$PATH:/usr/bin
PATH=$PATH:`cygpath -S`
PATH=$PATH:`cygpath -W`

# ------------------------
# alias
# ------------------------
alias h=history
alias ls="ls -C"

export http_proxy=http://lokiwang:qf6180QFV@proxy.tencent.com:8080
