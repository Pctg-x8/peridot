#!/bin/sh

cloneOrUpdateGitRepository() {
	if [ -e $2 ]
	then (cd $2; git pull -ff)
	else git clone $1 $2
	fi
}

[ -d extras ] || mkdir -p extras
cloneOrUpdateGitRepository https://github.com/pcwalton/pathfinder extras/pathfinder
