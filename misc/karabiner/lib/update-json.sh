#!/usr/bin/env bash

for srcfile in *.erb; do
  dstfile="$KARABINER_COMPLEX_MODIFICATIONS/`basename $srcfile .erb`"
  if [ "$srcfile" -nt "$dstfile" ]; then
    if /usr/bin/ruby lib/erb2json.rb < "$srcfile" > "$dstfile"; then
      echo "$dstfile"
    else
      rm -f "$dstfile"
      exit 1
    fi
  fi
done
