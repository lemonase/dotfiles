[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

appendpath () {
  if [ -d "$1" ]; then
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="${PATH:+$PATH:}$1"
    esac
  fi
}

appendpath "$HOME/.local/bin"
appendpath "$HOME/.local/scripts"

if command -v ruby > /dev/null && command -v gem > /dev/null; then
  appendpath "$(ruby -r rubygems -e 'puts Gem.user_dir')/bin"
fi

if command -v go > /dev/null; then
  export GOPATH="$HOME/.go"
  [ -d "$HOME/go" ] && mv "$HOME/go" "$HOME/.go"
  appendpath "$(go env GOPATH)/bin"
fi

if command -v cargo > /dev/null; then
  appendpath "$HOME/.cargo/bin"
fi

# vim:ft=sh
