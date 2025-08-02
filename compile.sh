#!/usr/bin/env bash

CLLSflags=""
Cflags="-std=c11"
onlyir=false
onlyast=false
debug=false
run=false
ofile=""

while getopts ":dOtPirasp:o:" opt; do
    case $opt in
        d)
            Cflags="$Cflags -g -O0"
            debug=true
            ;;
        O)
            Cflags="$Cflags -O1"
            ;;
        t)
            CLLSflags="$CLLSflags -t"
            ;;
        P)
            CLLSflags="$CLLSflags -P"
            ;;
        i)
            onlyir=true
            CLLSflags="$CLLSflags -i"
            ;;
        a)
            onlyast=true
            CLLSflags="$CLLSflags -a"
            ;;
        s)
            CLLSflags="$CLLSflags -s"
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
    echo "    -P: Compile with profiling enabled" >&2
    echo "    -i: Only generate IR code" >&2
    echo "    -a: Only generate AST" >&2
    echo "    -s: Disable concurrency" >&2
    echo "    -O: Compile with optimization flags" >&2
    echo "    -r: Run the compiled program after compilation" >&2
    echo "    -p <process>: Specify the name of the entry process" >&2
    echo "    -o <output>: Specify the output file (defaults to bin/<input>)" >&2
    exit 1
fi

if [ -z $ofile ]; then
    basename=$(basename $1)
    cfile=bin/${basename%.*}.c
    irfile=bin/${basename%.*}.ir
    astfile=bin/${basename%.*}.ast
    pfile=bin/${basename%.*}
    mkdir -p bin
else
    cfile=$ofile.c
    irfile=$ofile.ir
    astfile=$ofile.ast
    pfile=$ofile
fi

if [ $onlyast = true ]; then
    echo "@@@@@@ Generating AST to $astfile..." >&2 &&
    mvn -q exec:java -Dexec.args="$CLLSflags -c $1" > $astfile
elif [ $onlyir = true ]; then
    echo "@@@@@@ Generating IR code to $irfile..." >&2 &&
    mvn -q exec:java -Dexec.args="$CLLSflags -c $1" > $irfile
else
    echo "@@@@@@ Generating C code to $cfile..." >&2 &&
    mvn -q exec:java -Dexec.args="$CLLSflags -c $1" > $cfile &&
    echo "@@@@@@ Compiling $cfile to $pfile..." >&2 && 
    gcc $Cflags -o $pfile $cfile &&

    if [ $run = true ]; then
        echo "@@@@@@ Running $pfile:" >&2 &&
        if [ $debug = true ]; then
            gdb ./$pfile
        else
            ./$pfile
        fi
    fi
fi
