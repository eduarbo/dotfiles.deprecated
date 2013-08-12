rb_prompt(){
  if (( $+commands[rbenv] )); then
    version=$(rbenv version-name 2> /dev/null)
    if [[ "$version" == "" ]] then version="-" fi

    echo "$RUBY_PROMPT_PREFIX$version$RUBY_PROMPT_SUFFIX"
  elif (( $+commands[rvm-prompt] )); then
    echo "$RUBY_PROMPT_PREFIX$(rvm-prompt)$RUBY_PROMPT_SUFFIX"
  else
    echo ""
  fi
}
