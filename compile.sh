#!/usr/bin/env bash

CLLSflags=""
Cflags=""
debug=false
run=false
ofile=""

while getopts ":dOtrp:o:" opt; do
    case $opt in
        d)
            Cflags="-g"
            debug=true
            ;;
        O)
            Cflags="$Cflags -O1"
            ;;
        t)
            CLLSflags="$CLLSflags -t"
            ;;
        r)
            run=true
            ;;
        p)
            CLLSflags="$CLLSflags -p $OPTARG"
            ;;
        o)
            ofile=$OPTARG
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
    echo "Usage: $0 [-d] [-t] [-O] [-r] [-p <process>] [-o <output>] <input>" >&2
    echo "    -d: Compile with debug flags / Run with gdb" >&2
    echo "    -t: Compile with tracing enabled" >&2
    echo "    -O: Compile with optimization flags" >&2
    echo "    -r: Run the compiled program after compilation" >&2
    echo "    -p <process>: Specify the name of the entry process" >&2
    echo "    -o <output>: Specify the output file (defaults to bin/<input>)" >&2
    exit 1
fi

if [ -z $ofile ]; then
    basename=$(basename $1)
    cfile=bin/${basename%.*}.c
    pfile=bin/${basename%.*}
    mkdir -p bin
else
    cfile=$ofile.c
    pfile=$ofile
fi

echo "@@@@@@ Generating C code to $cfile..." &&
./CLLSj $CLLSflags -c $1 > $cfile &&
echo "@@@@@@ Compiling $cfile to $pfile..." && 
gcc $Cflags -o $pfile $cfile 

if [ $run = true ]; then
    echo "@@@@@@ Running $pfile:" &&
    if [ $debug = true ]; then
        gdb ./$pfile
    else
        ./$pfile
    fi
fi
