#!/usr/bin/env bash

for srcfile in $KARABINER_COMPLEX_MODIFICATIONS/*.json; do
  echo "check $srcfile"
  /usr/bin/ruby lib/lint.rb < "$srcfile" || exit 1
done
