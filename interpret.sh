#!/usr/bin/env bash

# This scripts includes the given source file and immediately runs the given process.

if [[ $# -lt 2 ]]; then
    echo "Usage: $0 source process" 
    exit 1
fi

source="$1"
process="$2"

java -cp target/classes pt.inescid.cllsj.CLLSj -i "include \"$source\";; $process();; quit;;"
