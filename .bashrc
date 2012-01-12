# ------------------------
# prompt
# ------------------------
if [ "$OSTYPE" == "cygwin" ] ; then
    if [ "$INSIDE_EMACS" == "" ] ; then
        export LC_ALL=zh_CN.gbk
    else
        export LC_ALL=zh_CN.utf-8
    fi
elif [ "$OSTYPE" == "linux" ] ; then
    export LC_ALL=zh_CN.utf-8
fi

if [ "$PS" == "" ] ; then
    export PS1="[\h \w]\$ "
    if [ "$TERM" == "xterm" ] ; then
        export PS1="\[\e[34m\]$PS1\[\e[0m\]"
    elif [ "$TERM" == "cygwin" ] ; then
        export PS1="\[\e[32;1m\]$PS1\[\e[0m\]"
    fi
fi

if [ "$TERM" == "xterm" ] ; then
    PROMPT_COMMAND='echo -ne "\e]0;${HOSTNAME} - ${PWD}\007"'
fi

# ------------------------
# for use in Emacs
# ------------------------
if [ ! "$INSIDE_EMACS" == "" ] ; then
    export GIT_PAGER=
fi

# ------------------------
# path
# ------------------------
export PATH=$HOME/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin/usr/x11R6/bin
export LD_LIBRARY_PATH=$HOME/lib:/usr/local/lib:/usr/lib:/lib
export MANPATH=$HOME/man:$HOME/share/man:/usr/local/man:/usr/local/share/man:/usr/man:/usr/share/man:$MANPATH
export INFOPATH=$HOME/info:$HOME/share/info:/usr/local/info:/usr/local/share/info:/usr/info:/usr/share/info:$INFOPATH

# ------------------------
# alias
# ------------------------
if [ ! "$EDITOR" == "" ] ; then
    alias vim=$EDITOR
fi

if [ -f "${HOME}/.bashrc_local" ] ; then
  source "${HOME}/.bashrc_local"
fi
