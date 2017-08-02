#!/usr/bin/env bash

for srcfile in src/json/*.erb; do
  dstfile=".config/karabiner/assets/complex_modifications/`basename $srcfile .erb`"
  if [ "$srcfile" -nt "$dstfile" ]; then
    if /usr/bin/ruby lib/erb2json.rb < "$srcfile" > "$dstfile"; then
      echo "$dstfile"
    else
      rm -f "$dstfile"
      exit 1
    fi
  fi
done
