#!/usr/bin/env bash

cfile=$(mktemp $(basename $1).XXXXX.c)
pfile=$(mktemp $(basename $1).XXXXX)
trap 'rm -f $cfile $pfile' EXIT

echo "@@@@@@ Running the code generator..." &&
./CLLSj -c < $1 > $cfile &&
echo "@@@@@@ Generated C code:" &&
cat $cfile &&
echo "@@@@@@ Compiling the C code..." &&
gcc -o $pfile $cfile &&
echo "@@@@@@ Running the executable:" &&
./$pfile
