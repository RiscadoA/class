#!/usr/bin/env bash

CLLSflags="--compile $CLLS_FLAGS"
Cflags="$C_FLAGS -std=c11"
dumpast=false
dumpiir=false
dumpfir=false
dumpanl=false
debug=false
run=false
ofile=""

while getopts ":aiIAOdro:" opt; do
    case $opt in
        a)
            dumpast=true
            ;;
        i)
            dumpiir=true
            ;;
        I)
            dumpfir=true
            ;;
        A)
            dumpanl=true
            ;;
        O)
            Cflags="$Cflags -O2"
            ;;
        d)
            Cflags="$Cflags -g -O1"
            debug=true
            ;;
        r)
            run=true
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
    echo "Usage: $0 [-a] [-i] [-I] [-A] [-O] [-d]  [-r] [-o <output>] <input>" >&2
    echo "    -a: Dump AST" >&2
    echo "    -i: Dump initial IR code" >&2
    echo "    -I: Dump final IR code" >&2
    echo "    -A: Dump IR analysis" >&2
    echo "    -O: Compile with GCC optimization flags" >&2
    echo "    -d: Compile with GCC debug flags / Run with gdb" >&2
    echo "    -r: Run the compiled program after compilation" >&2
    echo "    -o <output>: Specify the output file (defaults to bin/<input>)" >&2
    echo "Picks up other compilation flags from the CLLS_FLAGS environment variable" >&2
    exit 1
fi

if [ -z $ofile ]; then
    basename=$(basename $1)
    cfile=bin/${basename%.*}.c
    astfile=bin/${basename%.*}.ast
    irfile=bin/${basename%.*}.ir
    anlfile=bin/${basename%.*}.anl
    pfile=bin/${basename%.*}
    mkdir -p bin
else
    cfile=$ofile.c
    astfile=$ofile.ast
    irfile=$ofile.ir
    anlfile=$ofile.anl
    pfile=$ofile
fi

if [ $dumpast = true ]; then
    echo "@@@@@@ Dumping AST to $astfile..." >&2
    CLLSflags="$CLLSflags --output-ast=$astfile"
fi
if [ $dumpiir = true ]; then
    echo "@@@@@@ Dumping initial IR code to $irfile..." >&2
    CLLSflags="$CLLSflags --output-initial-ir=$irfile"
elif [ $dumpfir = true ]; then
    echo "@@@@@@ Dumping final IR code to $irfile..." >&2
    CLLSflags="$CLLSflags --output-final-ir=$irfile"
fi
if [ $dumpanl = true ]; then
    echo "@@@@@@ Dumping IR analysis to $anlfile..." >&2
    CLLSflags="$CLLSflags --output-analysis=$anlfile"
fi
CLLSflags="$CLLSflags --input=$1 --output-c=$cfile"

echo "@@@@@@ Using CLLS flags: $CLLSflags" >&2 &&
echo "@@@@@@ Dumping C code to $cfile..." >&2
mvn -q exec:java -e -Dexec.args="$CLLSflags" &&
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
