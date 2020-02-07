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
    if [ "$USER" == "root" ] ; then
        export PS1="[\h \w]\# "
    else
        export PS1="[\h \w]\$ "
    fi

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
# for cygwin
# ------------------------
if [ "$OSTYPE" == "cygwin" ] ; then
    export GIT_DISCOVERY_ACROSS_FILESYSTEM=1
fi

# ------------------------
# docker
# ------------------------
export DOCKER_OPTS='"--registry-mirror=https://registry.docker-cn.com"'

# ------------------------

# ------------------------
# for use in Emacs
# ------------------------
if [ ! "$INSIDE_EMACS" == "" ] ; then
    export GIT_PAGER=
fi

# ------------------------
# path
# ------------------------
export NO_PROXY=127.0.0.1:localhost

# ------------------------
# alias
# ------------------------
if [ ! "$EDITOR" == "" ] ; then
    alias vim=$EDITOR
fi

if [ -f "$HOME/.bashrc_local" ] ; then
  source "$HOME/.bashrc_local"
fi
