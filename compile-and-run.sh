#!/usr/bin/env bash

CLLSflags=""
Cflags=""
debug=false

while getopts ":dtp:" opt; do
    case $opt in
        p)
            CLLSflags="$CLLSflags -p $OPTARG"
            ;;
        d)
            Cflags="-g"
            debug=true
            ;;
        t)
            CLLSflags="$CLLSflags -t"
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done

shift $((OPTIND-1))

if [ -z $1 ]; then
    echo "Usage: $0 [-d] [-t] [-p <process>] <file>" >&2
    exit 1
fi

cfile=$(mktemp $(basename $1).XXXXX.c)
pfile=$(mktemp $(basename $1).XXXXX)
trap 'rm -f $cfile $pfile' EXIT

echo "@@@@@@ Running the code generator..." &&
./CLLSj $CLLSflags -c < $1 > $cfile &&
echo "@@@@@@ Generated C code:" &&
cat $cfile &&
echo "@@@@@@ Compiling the C code..." &&
gcc $Cflags -o $pfile $cfile &&
echo "@@@@@@ Running the executable:" &&
if [ $debug = true ]; then
    gdb ./$pfile
else
    ./$pfile
fi
