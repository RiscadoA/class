#!/usr/bin/env bash

cfile=$(mktemp $(basename $1).XXXXX.c)
pfile=$(mktemp $(basename $1).XXXXX)
trap 'rm -f $cfile $pfile' EXIT

if [ $# -eq 2 ] && [ $2 = "-d" ]; then
    flags="-g"
    debug="yes"
else
    flags=""
    debug="no"
fi

echo "@@@@@@ Running the code generator..." &&
./CLLSj -c < $1 > $cfile &&
echo "@@@@@@ Generated C code:" &&
cat $cfile &&
echo "@@@@@@ Compiling the C code..." &&
gcc $flags -o $pfile $cfile &&
echo "@@@@@@ Running the executable:" &&
if [ $debug = "yes" ]; then
    gdb ./$pfile
else
    ./$pfile
fi
