#!/bin/bash

function error() {
	echo $1
	exit 1
}

function usage() {

    echo "Usage: $0"
    exit 1
}

# [[ -n "$1" ]] || usage

# Test root
sudo last

for p in gups gups_lcg;
do
	echo $p
	make -s clean
	(make -s $p) || error "build failed"
	./$p 1G 4K 5

	echo "$p  (dune)"
	make -s clean
	(DUNE=1 make -s $p) || error "build failed"
	sudo ./$p 1G 4K 5
done
