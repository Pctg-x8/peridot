#!/bin/sh

IFS=$'\n' LINES=`grep unwrap **/*.rs -n | sed -e '/:[ \f\n\t\r]*\/\//d' -e 's/^\([^:]*\):\([0-9]*\):\(.*\)/Unwrap Found! Use expect with message instead. => \1:\2\\\x0a\3/p'`
if [[ ! -z $LINES ]]; then (echo $LINES; exit 1); else echo "No unwraps found"; fi
