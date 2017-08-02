#!/usr/bin/env bash

for srcfile in .config/karabiner/assets/complex_modifications/*.json; do
  echo "check $srcfile"
  /usr/bin/ruby lib/lint.rb < "$srcfile" || exit 1
done
