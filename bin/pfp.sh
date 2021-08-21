#!/bin/sh

HOGE=" "
mpif90="mpif90"

if [ "$1" = "install" ]; then
    HOGE="$2"
    fname="${HOGE##*/}"
    curl -XGET "$2" -k --output "./tmp/$fname"
    $mpif90 -c inc/*o -I ./inc "./tmp/$fname" -fopenmp -fopenacc -g -fcheck=all  -fintrinsic-modules-path inc/ -o ./inc
fi