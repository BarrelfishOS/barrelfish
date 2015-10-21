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

for size in 32G; do
#2M 4M 8M 16M 32M 64M 128M; do

    echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    echo "Running size $size"

    for p in gups #gups_lcg #gups
    do
	echo "bench is: $p"
	make -s clean
	(make -s $p) &>/dev/null || error "build failed"
	sudo taskset --cpu-list 1 chrt 99 numactl -m 0 ./$p $size 2M 1 

	#echo "bench is: $p  (dune)"
	#make -s clean
	#(DUNE=1 make -s $p) &>/dev/null || error "build failed"
	#sudo ./$p $size 2M 5
    done
done
