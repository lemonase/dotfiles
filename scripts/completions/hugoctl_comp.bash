# bash completion for hugctl

_hugctl_completions() {
  local cur
  cur="${COMP_WORDS[COMP_CWORD]}"

  local blog_dir="$HOME/src/blog"
  case ${COMP_CWORD} in
    1)
      OPTS="new list edit serve kill deploy"
      compopt -o bashdefault -o default
      COMPREPLY=( $(compgen -W "${OPTS[*]}" -- $cur) )
      ;;
    2)
      case ${COMP_CWORD[2]} in
        *)
        COMPREPLY=($(compgen -W "$(find $blog_dir/content/posts/ -iname '*.md' -type f | xargs -I@ basename @)" -- $cur));
          ;;
      esac
      ;;
  esac
  return 0
}

complete -F _hugctl_completions hugctl
