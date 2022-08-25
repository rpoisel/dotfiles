delete_docker_containers () {
  docker_containers=$(docker ps -aq --format '{{.ID}} {{.Names}}' | awk '{if ($2 !~ /k3d-acme/) { print $1 } }')
  if ! [ "${docker_containers}" = "" ]; then
    docker rm -f ${docker_containers}
  fi
  docker network prune -f
  docker volume prune -f
}

delete_all_docker_containers() {
  docker_containers=$(docker ps -aq)
  if ! [ "${docker_containers}" = "" ]; then
    docker rm -f ${docker_containers}
  fi
  docker network prune -f
  docker volume prune -f
}

ORIGPATH=${PATH}

goversion() {
  if [ -z $1 ]; then
    export PATH=${ORIGPATH}
  else
    export PATH=/usr/lib/go-"${1}"/bin:${PATH}
  fi
}

# system
alias rc-packages="dpkg -l | grep ^rc | awk '{ print $2 }' | xargs"
alias drrc="sudo apt purge $(rc-packages)"

# editor
alias enw="emacs -nw"
alias ecl="emacsclient"
alias vim="emacs -nw"
alias vvim=/usr/bin/vim

# fun stuff
alias please="sudo"

# other stuff
alias ff="find . -type f -name"

# shell
alias histoff="set +o history"
alias histon="set -o history"

# git
alias gg="git cola 2>/dev/null"
alias gap="git add -p"
alias gs="git status"
alias gca="git commit --amend --no-edit"
alias gcae="git commit --amend"
alias gacar="git add . && git commit --amend --no-edit && git review -v"
alias gpp="git pull -p"
alias gl="git log"
alias gls="git log --stat"
alias gr="git review -v"
alias reset-hard="git reset --hard origin/master && git checkout -- . && git checkout master && git reset --hard origin/master && git pull && git submodule foreach --recursive git reset --hard && git submodule update --init --recursive"
alias grm="git rebase master"
alias gsu="git submodule update --init --recursive"

# maven
alias mvci="mvn clean install"
alias mvcis="mvn clean install -Dpmd.skip -DcheckStyle.skip -DskipTests"
alias dpsa="docker ps -a"
alias drmf="delete_docker_containers"
alias drmfa="delete_all_docker_containers"
alias drm="docker rm -f"
alias kc="kubectl"
alias xclip="xclip -selection clipboard"
alias rpo="rpoisel"

# cluster
alias dkc="docker compose"
complete -F _complete_alias dkc

alias kc=kubectl
complete -F __start_kubectl kc
alias kcd="KUBECTL_EXTERNAL_DIFF=meld kubectl diff -f <(kustomize build .)"
alias kctx="kubectx"
alias kns="kubens"

alias ku=kustomize
alias kub="kustomize build . | yq eval -C"
alias kubl="kustomize build . | yq eval -C | less -R"
