#!/bin/sh

if [ -e extras/pathfinder ]; then
	cd extras/pathfinder; git pull -ff; cd ../..
else
	git clone https://github.com/pcwalton/pathfinder extras/pathfinder
fi

