#!/usr/bin/env bash

function success {
    if [ -t 1 ]; then
        echo -e "\e[32m$1\e[0m"
    else
        echo $1
    fi
}

function error {
    if [ -t 1 ]; then
        echo -e "\e[31m$1\e[0m"
    else
        echo $1
    fi
}

# Find all *.clls files in the tests directory, recursively
for file in $(find tests -name "*.clls");
do
    # Check if there are accompanying .trace or .out files
    basename=$(basename $file .clls)
    outfile=$(dirname $file)/$basename.out

    baseout=bin/$(dirname $file)/$basename
    flags="-P -o $baseout"
    mkdir -p $(dirname $baseout)

    if [ ! -f $outfile ]; then
        error "@@@@@@ Test $file missing expected output file"
        continue
    fi

    # Compile the file
    echo -n "@@@@@@ Compiling $file... "
    ./compile.sh $flags $file 2> $baseout.err
    if [ $? -ne 0 ]; then
        error "failed! See $baseout.err"
        continue
    fi
    success "done"

    # Run the compiled file
    echo -n "@@@@@@ Running $file... "
    $baseout > $baseout.out 2> $baseout.err
    if [ $? -ne 0 ]; then
        error "failed! See $baseout.out and $baseout.err"
        continue
    fi

    # Compare the output with the expected
    diff $baseout.out $outfile > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        error "expected $outfile, got $baseout.out"
        continue
    else
        success "passed"
    fi
done
