if [ -n "${BASH_VERSION}" ]; then
  if [ -f "${HOME}/.bashrc" ]; then
    source "${HOME}/.bashrc"
  fi
fi
export LANG="zh_CN.UTF-8"
export LANGUAGE="en"
export LC_MESSAGES="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_COLLATE="en_US.UTF-8"

if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi
