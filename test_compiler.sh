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
    # Check if file matches argument regex, if any
    if [ ! -z $1 ]; then
        if [[ ! $file =~ $1 ]]; then
            continue
        fi
    fi

    # Check if there are accompanying .trace or .out files
    basename=$(basename $file .clls)
    outfile=$(dirname $file)/$basename.out

    baseout=bin/$(dirname $file)/$basename
    flags="-t -P -o $baseout"
    mkdir -p $(dirname $baseout)

    if [ ! -f $outfile ]; then
        error "@@@@@@ Skipping $file: missing expected output file $outfile"
        continue
    fi

    # Compile the file
    echo -n "@@@@@@ Compiling $file... "
    ./compile.sh $flags $file &> $baseout.err
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
